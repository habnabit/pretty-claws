// © 2025 <_@habnab.it>
//
// SPDX-License-Identifier: EUPL-1.2

use std::marker::PhantomData;

use bevy::{
    animation::{AnimationTarget, AnimationTargetId, RepeatAnimation},
    prelude::*,
};
use petgraph::graph::NodeIndex;

pub trait SavedAnimationNode {
    type AnimatedFrom: Component;
    fn node_mut(&mut self) -> &mut Option<NodeIndex>;
}

#[derive(Debug)]
pub struct AnimatorPlugin<T>(PhantomData<fn() -> T>);

impl<T> Default for AnimatorPlugin<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

type AnimD<'r, C> = (
    &'r AnimationTarget,
    &'r mut C,
    &'r <C as SavedAnimationNode>::AnimatedFrom,
);
type ReaderD<'r> = (&'r mut AnimationPlayer, &'r AnimationGraphHandle);
type Clips = Assets<AnimationClip>;
type Graphs = Assets<AnimationGraph>;
type CB<C> = Box<dyn FnOnce(Query<AnimD<C>>, Query<ReaderD>, ResMut<Clips>, ResMut<Graphs>) + Send>;

impl<C: SavedAnimationNode + Component> AnimatorPlugin<C> {
    pub fn start_animation<F>(
        commands: &mut Commands,
        entity: Entity,
        repeat: RepeatAnimation,
        build_clip: F,
    ) where
        F: FnOnce(&C::AnimatedFrom, AnimationTargetId) -> AnimationClip + Send + 'static,
    {
        let cb: CB<C> = Box::new(
            move |mut q_animation, mut q_reader, mut animation_clips, mut animation_graphs| {
                let Some((target, mut saved, anim_from, mut player, graph)) = (try {
                    let (target, saved, anim_from) = q_animation.get_mut(entity).ok()?;
                    let (player, graph_handle) = q_reader.get_mut(target.player).ok()?;
                    let graph = animation_graphs.get_mut(graph_handle.id())?;
                    (target, saved, anim_from, player, graph)
                }) else {
                    warn!("couldn't start a readied animation");
                    return;
                };
                let clip = build_clip(anim_from, target.id);
                if let &mut Some(prev_node) = saved.node_mut() {
                    graph.remove_edge(graph.root, prev_node);
                }
                let clip_handle = animation_clips.add(clip);
                let node_index = graph.add_clip(clip_handle, 1., graph.root);
                player.play(node_index).set_repeat(repeat);
                *saved.node_mut() = Some(node_index);
            },
        );
        commands.run_system_cached_with(
            move |In(callback): In<CB<C>>,
                  q_animation: Query<AnimD<C>>,
                  q_reader: Query<ReaderD>,
                  animation_clips: ResMut<Clips>,
                  animation_graphs: ResMut<Graphs>| {
                callback(q_animation, q_reader, animation_clips, animation_graphs);
            },
            cb,
        );
    }
}
