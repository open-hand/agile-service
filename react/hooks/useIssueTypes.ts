import {
  useCallback, useState, useEffect,
} from 'react';
import { stores } from '@choerodon/boot';
import { issueTypeApi } from '@/api';
import { remove } from 'lodash';
import { IIssueType } from '@/common/types';

const { AppState } = stores;

interface Config {
  disabled?: boolean
}
export default function useIssueTypes(config?: Config): [IIssueType[], Function] {
  const [data, setData] = useState<IIssueType[]>([]);
  const type = AppState.currentMenuType.category === 'PROGRAM' ? 'program' : 'agile';
  const isProgram = type === 'program';
  const refresh = useCallback(async () => {
    const res = await issueTypeApi.loadAllWithStateMachineId(type);
    const epicType = remove(res, (item) => item.typeCode === 'issue_epic');
    // @ts-ignore
    if (epicType && epicType.length) {
      res.push(epicType[0]);
    }
    setData(!isProgram ? res.filter((item: IIssueType) => item.typeCode !== 'feature') : res);
  }, [type, isProgram]);
  useEffect(() => {
    if (!config?.disabled) {
      refresh();
    }
  }, [config?.disabled, refresh]);

  return [data, refresh];
}
