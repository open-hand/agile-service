import { Fragment } from 'react';

const Empty = Fragment;
const InjectedComponent = {
  Backlog: Empty,
};
export function inject(key, Component) {
  InjectedComponent[key] = Component;
}
export { InjectedComponent };
