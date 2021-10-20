import React, {
  createContext, useEffect, useMemo, useRef,
} from 'react';
import { inject } from 'mobx-react';
import { injectIntl } from 'react-intl';
import EditIssueStore from './EditIssueStore';
import { getMenuType } from '@/utils/common';

const EditIssueContext = createContext();
export default EditIssueContext;
export const EditIssueContextProvider = injectIntl(inject('AppState', 'HeaderStore')((props) => {
  const FieldVersionRef = {
    current: null,
  };
  const FieldFixVersionRef = {
    current: null,
  };
  const isProjectLevel = useMemo(() => (props.menuType || getMenuType()) === 'project', [props.menuType, getMenuType]);
  const descriptionEditRef = useRef(false);
  const value = {
    ...props,
    isProjectLevel,
    menuType: props.menuType || getMenuType(), /** project organization */
    prefixCls: 'c7n-agile-EditIssue',
    intlPrefix: 'agile.EditIssue',
    store: useMemo(() => new EditIssueStore({ projectId: props.projectId }), [props.projectId]), // 防止update时创建多次store
    FieldVersionRef,
    FieldFixVersionRef,
    descriptionEditRef,
    saveFieldVersionRef: (ref) => {
      FieldVersionRef.current = ref;
    },
    saveFieldFixVersionRef: (ref) => {
      FieldFixVersionRef.current = ref;
    },
  };

  useEffect(() => {
    value.store.initApi(props.outside, props.organizationId, props.projectId);
    return () => {
      value.store.destroy();
    };
  }, [props.organizationId, props.outside, props.projectId, value.store]);

  useEffect(() => {
    value.store.setTab(props.tab);
  }, [props.tab, value.store]);
  return (
    <EditIssueContext.Provider value={value}>
      {props.children}
    </EditIssueContext.Provider>
  );
}));
