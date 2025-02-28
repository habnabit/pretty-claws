// © 2025 <_@habnab.it>
//
// SPDX-License-Identifier: EUPL-1.2

#![feature(try_blocks, cmp_minmax, lazy_get)]

mod animation;

use std::{any::TypeId, cell::LazyCell, f32::consts::PI, time::Duration};

use animation::{AnimatorPlugin, SavedAnimationNode};
use bevy::{
    animation::{
        animated_field, AnimationEntityMut, AnimationEvaluationError, AnimationTarget,
        AnimationTargetId, RepeatAnimation,
    },
    color::palettes::css,
    input::common_conditions::input_just_pressed,
    prelude::*,
    sprite::Anchor,
    utils::hashbrown::{HashMap, HashSet},
    window::PrimaryWindow,
};
use bevy_inspector_egui::quick::WorldInspectorPlugin;
use petgraph::graph::NodeIndex;
use rand::{distr::Distribution, seq::SliceRandom, Rng, SeedableRng};
use rand_chacha::ChaCha8Rng;
use uuid::Uuid;

const NO_PICK: PickingBehavior = PickingBehavior {
    should_block_lower: false,
    is_hoverable: false,
};

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .init_resource::<SeededRng>()
        .add_plugins(WorldInspectorPlugin::new())
        .register_type::<SeededRng>()
        .add_systems(Startup, setup)
        .add_systems(
            Update,
            (
                spawn_fox_paws.run_if(run_once),
                spin_fox_paws.run_if(input_just_pressed(KeyCode::KeyS)),
            ),
        )
        .run();
}

#[derive(Resource, Reflect)]
#[reflect(Resource)]
#[reflect(from_reflect = false)]
struct SeededRng(#[reflect(ignore)] ChaCha8Rng);

impl FromReflect for SeededRng {
    fn from_reflect(_reflect: &dyn PartialReflect) -> Option<Self> {
        todo!()
    }
}

impl FromWorld for SeededRng {
    fn from_world(_world: &mut World) -> Self {
        SeededRng(ChaCha8Rng::from_os_rng())
    }
}

static FOX_PAW_IMAGES: &[&'static str] = &[
    "paw2.png",
    "paw2-1.png",
    "paw2-2.png",
    "paw2-3.png",
    "paw2-4.png",
    "paw2-5.png",
];
static FOX_PAW_SIZE: Vec2 = Vec2::new(172.13, 250.);

#[derive(Resource, Reflect)]
#[reflect(Resource)]
struct FoxPawSlices {
    images: Vec<Handle<Image>>,
}

#[derive(Debug, Default, Component, Reflect)]
struct FoxPaws {
    rotated: bool,
}

#[derive(Reflect, Debug, Component, Clone, Default)]
struct FoxPawsSpinNode(Option<NodeIndex>);

impl SavedAnimationNode for FoxPawsSpinNode {
    type AnimatedFrom = Transform;

    fn node_mut(&mut self) -> &mut Option<NodeIndex> {
        &mut self.0
    }
}

fn spawn_fox_paws(
    q_player: Query<Entity, With<AnimationPlayer>>,
    slices: Res<FoxPawSlices>,
    mut commands: Commands,
) {
    let Ok(player) = q_player.get_single() else {
        return;
    };
    let spawn = |parent: &mut ChildBuilder, flip_x, transform| {
        parent
            .spawn((
                Sprite {
                    image: slices.images[0].clone(),
                    flip_x,
                    custom_size: Some(FOX_PAW_SIZE),
                    ..Default::default()
                },
                transform,
            ))
            .with_children(|parent| {
                for image in &slices.images[1..] {
                    parent.spawn((
                        Sprite {
                            image: image.clone(),
                            flip_x,
                            custom_size: Some(FOX_PAW_SIZE),
                            ..Default::default()
                        },
                        Transform::from_xyz(0., 0., -1.),
                    ));
                }
            });
    };
    commands
        .spawn((
            FoxPaws::default(),
            FoxPawsSpinNode::default(),
            AnimationTarget {
                id: AnimationTargetId(Uuid::new_v4()),
                player,
            },
            Transform::default(),
            InheritedVisibility::VISIBLE,
        ))
        .with_children(|parent| {
            spawn(parent, true, Transform::from_xyz(-100., 0., 0.));
            spawn(parent, false, Transform::from_xyz(100., 0., 0.));
        });
}

fn spin_fox_paws(
    mut q_paws: Query<(Entity, &mut FoxPaws), With<AnimationTarget>>,
    mut commands: Commands,
) {
    let Ok((paw_entity, mut paws)) = q_paws.get_single_mut() else {
        return;
    };
    paws.rotated = !paws.rotated;
    let rotation = Quat::from_rotation_z(if paws.rotated { PI } else { 0. });
    AnimatorPlugin::<FoxPawsSpinNode>::start_animation(
        &mut commands,
        paw_entity,
        RepeatAnimation::Never,
        move |transform, target| {
            let mut clip = AnimationClip::default();
            clip.add_curve_to_target(
                target,
                AnimatableCurve::new(
                    animated_field!(Transform::rotation),
                    EasingCurve::new(transform.rotation, rotation, EaseFunction::CubicInOut)
                        .reparametrize_linear(interval(0., 0.5).unwrap())
                        .unwrap(),
                ),
            );
            clip
        },
    );
}

#[derive(Resource, Reflect)]
#[reflect(Resource)]
struct UIBorders {
    texture: Handle<Image>,
    atlas_layout: Handle<TextureAtlasLayout>,
    slicer: TextureSlicer,
}

impl UIBorders {
    fn make_sprite(&self, index: usize, color: Color) -> Sprite {
        let mut sprite = Sprite::from_atlas_image(self.texture.clone(), TextureAtlas {
            index,
            layout: self.atlas_layout.clone(),
        });
        sprite.color = color;
        sprite.image_mode = SpriteImageMode::Sliced(self.slicer.clone());
        sprite
    }
}

fn setup(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut texture_atlases: ResMut<Assets<TextureAtlasLayout>>,
    // mut rng: ResMut<SeededRng>,
    mut animation_graphs: ResMut<Assets<AnimationGraph>>,
) {
    commands.spawn(Camera2d);
    commands.spawn((
        AnimationPlayer::default(),
        AnimationGraphHandle(animation_graphs.add(AnimationGraph::new())),
    ));

    commands.insert_resource({
        let images = FOX_PAW_IMAGES
            .iter()
            .map(|&path| asset_server.load(path))
            .collect();
        FoxPawSlices { images }
    });

    commands.insert_resource({
        let texture = asset_server.load("fantasy_ui_border_sheet.png");
        let atlas_layout =
            TextureAtlasLayout::from_grid(UVec2::new(50, 50), 6, 6, Some(UVec2::splat(2)), None);
        let atlas_layout = texture_atlases.add(atlas_layout);
        let slicer = TextureSlicer {
            border: BorderRect::square(24.0),
            center_scale_mode: SliceScaleMode::Stretch,
            sides_scale_mode: SliceScaleMode::Stretch,
            max_corner_scale: 1.0,
        };
        UIBorders {
            texture,
            atlas_layout,
            slicer,
        }
    });
}
