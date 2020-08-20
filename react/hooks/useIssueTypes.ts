import {
  useCallback, useState, useEffect,
} from 'react';
import { stores } from '@choerodon/boot';
import { issueTypeApi } from '@/api';
import { IIssueType } from '@/common/types';

const { AppState } = stores;

export default function useIssueTypes(): [IIssueType[], Function] {
  const [data, setData] = useState<IIssueType[]>([]);
  const type = AppState.currentMenuType.category === 'PROGRAM' ? 'program' : 'agile';
  const isProgram = type === 'program';
  const refresh = useCallback(async () => {
    const res = await issueTypeApi.loadAllWithStateMachineId(type);
    setData(!isProgram ? res.filter((item: IIssueType) => item.typeCode !== 'feature') : res);
  }, [type, isProgram]);
  useEffect(() => {
    refresh();
  }, [refresh]);

  return [data, refresh];
}
