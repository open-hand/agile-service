import React, {
  createContext, useMemo, useContext,
} from 'react';
import { inject } from 'mobx-react';
import {
  DataSet,
} from 'choerodon-ui/pro';
import { useCreation, useMount } from 'ahooks';
import { groupBy, omit } from 'lodash';
import GanntDependencyModalDataSet from './GanntDependencyModalDataSet';
import { IModalProps } from '@/common/types';
import type { IGanttUpdateIssueDependencyItem } from '@/api';
import { GanttIssue } from '@/routes/gantt/types';

export interface IGanttDependencyModalProps {
  onOk?: (data: IGanttUpdateIssueDependencyItem[]) => void
  data?: Array<{ id: string, issueId: string, organizationId: string, predecessorId: string } & Required<Pick<GanttIssue, 'predecessorType'>>>
  issueId: string
  title?: string,
  forwardRef?: any,
  disableAutoCreate?: boolean,
  horizontal?: boolean,
  otherArgs?: object,
}

interface Context extends Omit<IGanttDependencyModalProps, 'data'> {
  dataset: DataSet
  data: { [key: string]: string[] }
  modal?: IModalProps
}
const Store = createContext({} as Context);

export default function useGanntDependencyModal() {
  return useContext(Store);
}
export const StoreProvider = inject('AppState')(
  (props: IGanttDependencyModalProps & { children: any }) => {
    const { children, data, disableAutoCreate = false } = props;
    const editData = useCreation(() => Object.entries(groupBy(data || [], (item) => item.predecessorType))
      .reduce((pre, [predecessorType, issues]) => ({ ...pre, [predecessorType]: issues.flat().map((item) => item.predecessorId) }), {}) as { [key: string]: string[] }, []);
    const dataset = useCreation(() => new DataSet(GanntDependencyModalDataSet(Object.entries(editData).map(([predecessorType, predecessorId]) => ({
      predecessorType,
      predecessorId,
    })))), []);
    useMount(() => {
      !disableAutoCreate && !data?.length && dataset.create({ predecessorType: 'predecessor_fs' });
    });

    return (
      <Store.Provider value={{ ...omit(props, 'data'), data: editData || {}, dataset }}>
        {children}
      </Store.Provider>
    );
  },
);
