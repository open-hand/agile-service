import { createContext, useContext } from 'react';
import { stores } from '@choerodon/boot';
import { issueTypeApi } from '@/api';
import { IIssueType } from '@/common/types';

const { AppState } = stores;
interface Context {
  data: IIssueType[]
  refresh: () => Promise<IIssueType[]>
}
const IssueTypeContext = createContext<Context>({
  data: [],
  refresh: async () => {
    const type = AppState.currentMenuType.category === 'PROGRAM' ? 'program' : 'agile';
    const isProgram = type === 'program';
    const data = await issueTypeApi.loadAllWithStateMachineId(type);
    return !isProgram ? data.filter((item:IIssueType) => item.typeCode !== 'feature') : data;
  },
});
export { IssueTypeContext };

export default function useIssueTypes():[Context['data'], Context['refresh']] {
  const { data, refresh } = useContext(IssueTypeContext) || {};
  return [data, refresh];
}
