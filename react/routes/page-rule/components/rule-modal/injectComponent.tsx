import { Fragment } from 'react';

const Empty = Fragment;
const InjectedComponent = {
  BacklogType: Empty,
  BacklogClassification: Empty,
  Urgent: Empty,
};
export function pageRuleInject(key: 'BacklogType' | 'BacklogClassification' | 'Urgent', Component: any) {
  InjectedComponent[key] = Component;
}
export { InjectedComponent };
