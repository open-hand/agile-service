import React, {
  createContext, useState, useContext, useMemo,
} from 'react';
import { observer } from 'mobx-react-lite';
import { IFiledProps } from '@/api';

interface Context {
  disabled: boolean | undefined,
  showSplitLine?: boolean,
  isProject: boolean,
  onDelete?: (data: IFiledProps) => void,
  prefixCls: 'c7n-page-issue-detail',
}

const SortTableContext = createContext({} as Context);

export function useSortTableContext() {
  return useContext(SortTableContext);
}

const SortTableProvider = observer(
  (props: any) => {
    const value = {
      ...props,
      prefixCls: 'c7n-page-issue-detail',
    };
    return (
      <SortTableContext.Provider value={value}>
        {props.children}
      </SortTableContext.Provider>
    );
  },
);
export default SortTableProvider;
