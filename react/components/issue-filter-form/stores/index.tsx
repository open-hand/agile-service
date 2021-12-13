import React, {
  createContext, useContext, useEffect, useMemo, useState,
} from 'react';
import {
  DataSet,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { useCreation, usePersistFn } from 'ahooks';
import { IIssueFilterFormProps } from '..';
import IssueFilterFormDataSet from './IssueFilterFormDataSet';
import { initFieldIssueFilterForm } from '../utils';

interface Context extends IIssueFilterFormProps {
  footer?: React.ReactNode,
  dataSet: DataSet
  extraRenderFields: Required<IIssueFilterFormProps>['extraRenderFields']
  prefixCls: string,
}
const IssueFilterFormStoreContext = createContext({} as Context);

export function useIssueFilterFormStore() {
  return useContext(IssueFilterFormStoreContext);
}
const IssueFilterFormStoreContextProvider = observer(
  (props: IIssueFilterFormProps & Context & { children: React.ReactElement }) => {
    const currentFormCode = useCreation(() => new Map<'chosenFields' | 'extraFormItems', Set<string>>([['chosenFields', new Set()], ['extraFormItems', new Set()]]), []);
    const dataSet = useMemo(() => {
      if (props.dataSet) {
        return props.dataSet;
      }
      return new DataSet(IssueFilterFormDataSet({ fields: props.fields || [] }));
    }, [props.dataSet, props.fields]);

    useEffect(() => {
      props.extraFormItems?.forEach((field) => {
        !currentFormCode.get('extraFormItems')?.has(field.code)
          && currentFormCode.get('extraFormItems')?.add(field.code) && initFieldIssueFilterForm(field, dataSet);
      });
    }, [currentFormCode, dataSet, props.extraFormItems]);
    useEffect(() => {
      if (props.needInit) {
        // 初始化值
        props.chosenFields?.forEach((field) => {
          !currentFormCode.get('chosenFields')?.has(field.code)
            && currentFormCode.get('chosenFields')?.add(field.code) && initFieldIssueFilterForm(field, dataSet);
        });
      }
    }, [currentFormCode, dataSet, props.chosenFields, props.needInit]);
    const extraRenderFields: Context['extraRenderFields'] = usePersistFn((...args) => {
      if (props.extraRenderFields) {
        return props.extraRenderFields(...args);
      }
      return false;
    });
    const value = {
      ...props,
      dataSet,
      extraRenderFields,
      prefixCls: 'c7n-agile-export-issue-modal',
    };
    return (
      <IssueFilterFormStoreContext.Provider value={value}>
        {props.children}
      </IssueFilterFormStoreContext.Provider>
    );
  },
);
export default IssueFilterFormStoreContextProvider;
