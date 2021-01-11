import { Fragment } from 'react';

const Empty = Fragment;
const InjectedRenderComponent = {
  backlogType: Empty,
  backlogClassification: Empty,
  progressFeedback: Empty,
};
export function inject(key, Component) {
  InjectedRenderComponent[key] = Component;
}
export { InjectedRenderComponent };
