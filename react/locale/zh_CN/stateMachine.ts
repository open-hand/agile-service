import { localeAppendPrefixObjectKey } from '@/utils/locale';

const locale = {
  route: '状态机',
  state: '状态',
  name: '名称',
  stage: '阶段',
  'solve.status': '是否为已解决',
  usage: '使用情况',
  'create.state': '创建状态',
  flow: '状态与流转',
  'flow.init.state': '设置初始状态',
  'flow.create.new.state': '创建新的状态',
  'flow.add.exist.state': '添加已有状态',
  customFlow: '自定义流转',
  'customFlow.log': '查看执行日志',

} as const;
const exportStateMachine = localeAppendPrefixObjectKey({ intlPrefix: 'stateMachine' as const, intlObject: locale });
type ILocaleStateMachineType = {
  ['agile.stateMachine']: Array<keyof typeof locale>[number]
}
export { exportStateMachine };
export type { ILocaleStateMachineType };
