import React, { createContext, useContext, useMemo } from 'react';
import { inject } from 'mobx-react';
import { DataSet } from 'choerodon-ui/pro';
import { DataSetSelection } from 'choerodon-ui/pro/lib/data-set/enum';
import FormDataSet from './FormDataSet';

interface ContextProps {
  intlPrefix: string,
  formDs: DataSet,
}

const Store = createContext({} as ContextProps);

export function useRemindSettingStore() {
  return useContext(Store);
}

export const StoreProvider = inject('AppState')((props: any) => {
  const {
    children,
    issueId,
  } = props;
  const intlPrefix = 'c7nag.work.calendar.remind';
  const typeDs = useMemo(() => new DataSet({
    data: [{
      text: '开始时',
      value: 'at_start',
    }, {
      text: '开始前',
      value: 'before_start',
    }, {
      text: '开始后',
      value: 'after_start',
    }, {
      text: '结束时',
      value: 'at_end',
    }, {
      text: '结束前',
      value: 'before_end',
    }, {
      text: '结束后',
      value: 'after_end',
    }, {
      text: '自定义时间',
      value: 'custom_time',
    }],
    selection: DataSetSelection.single,
  }), []);
  const unitDs = useMemo(() => new DataSet({
    data: [{
      text: '分钟',
      value: 'minute',
    }, {
      text: '小时',
      value: 'hour',
    }, {
      text: '天',
      value: 'day',
    }],
    selection: DataSetSelection.single,
  }), []);
  const formDs = useMemo(() => new DataSet(FormDataSet({
    issueId, typeDs, unitDs,
  })), []);

  const value = {
    ...props,
    intlPrefix,
    formDs,
  };
  return (
    <Store.Provider value={value}>
      {children}
    </Store.Provider>
  );
});
