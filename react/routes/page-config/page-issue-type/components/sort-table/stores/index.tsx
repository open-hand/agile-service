import React, {
  createContext, useState, useContext, useMemo,
} from 'react';
import { injectIntl, InjectedIntl } from 'react-intl';
import { inject } from 'mobx-react';
import { DataSet } from 'choerodon-ui/pro/lib';
import { observer } from 'mobx-react-lite';
import { IFiledProps } from '@/api';

interface Context {
  disabled: boolean | undefined,
  showSplitLine?: boolean,
  onDelete?: (data: IFiledProps) => void,
}

const SortTableContext = createContext({} as Context);

export function useSortTableContext() {
  return useContext(SortTableContext);
}

const SortTableProvider = observer(
  (props: any) => {
    const value = {
      ...props,
    };
    return (
      <SortTableContext.Provider value={value}>
        {props.children}
      </SortTableContext.Provider>
    );
  },
);
export default SortTableProvider;
