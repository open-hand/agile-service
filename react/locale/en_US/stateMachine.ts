import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  route: 'Workflow',
  state: 'States',
  name: 'Name',
  stage: 'Stage',
  'solve.status': 'Choose whether it is resolved',
  usage: 'Usage',
  'create.state': 'Create',
  flow: 'States And Flow',
  'flow.init.state': 'Set Initial States',
  'flow.create.new.state': 'Create',
  'flow.add.exist.state': 'Add',
  customFlow: 'Custom Flow',
  'customFlow.log': 'Audit log',

} as const;
const exportStateMachine = localeAppendPrefixObjectKey({ intlPrefix: 'stateMachine' as const, intlObject: locale });
type ILocaleStateMachineType = {
  ['agile.stateMachine']: Array<keyof typeof locale>[number]
}
export { exportStateMachine };
export type { ILocaleStateMachineType };
