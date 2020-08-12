/* eslint-disable react-hooks/rules-of-hooks */
import React, {
  useContext, useRef, useEffect, useState,
} from 'react';
import { stores } from '@choerodon/boot';
import { DefaultPriorityContext } from '@/hooks/useDefaultPriority';
import { PriorityContext } from '@/hooks/usePriorities';
import { IsOwnerContext } from '@/hooks/useIsOwner';

const { AppState } = stores;
function wrapWithContexts(contexts, values, children) {
  return contexts.reduce((last, Context, index) => (
    <Context.Provider value={values.get(Context)}>
      {last}
    </Context.Provider>
  ), children);
}
const AgileProvider = (contexts) => function AgileDataProvider({ children, projectId }) {
  const dataRef = useRef(new Map());
  contexts.forEach((context, index) => {
    const { data: initData, refresh } = useContext(context);
    const [data, setData] = useState(initData);

    const loadData = async (...args) => {
      if (AppState.currentMenuType?.type === 'project') {
        const res = await refresh(...args);
        dataRef.current.set(context, { data: res, refresh: loadData });
        setData(res);
      }
    };
    // 初始化
    if (!dataRef.current.get(context)) {
      dataRef.current.set(context, { data, refresh: loadData });
    }
    useEffect(() => {
      loadData();
    }, [projectId]);
  });
  return wrapWithContexts(contexts, dataRef.current, children);
};

export default AgileProvider([
  DefaultPriorityContext,
  PriorityContext,
  IsOwnerContext,
]);
