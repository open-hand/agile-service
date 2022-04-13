import React, {
  createContext, useEffect, useMemo, useRef,
} from 'react';
import { inject } from 'mobx-react';
import { observer } from 'mobx-react-lite';
import EditIssueStore from './EditIssueStore';
import { getMenuType, getProjectId } from '@/utils/common';
import useIsInProgram from '@/hooks/useIsInProgram';
import useIsProgram from '@/hooks/useIsProgram';
import useIsProgramIssueType from '@/hooks/useIsProgramIssueType';

const EditIssueContext = createContext();
export default EditIssueContext;
export const EditIssueContextProvider = inject('AppState', 'HeaderStore')(observer((props) => {
  const FieldVersionRef = {
    current: null,
  };
  const FieldFixVersionRef = {
    current: null,
  };
  const isProjectLevel = useMemo(() => (props.menuType || getMenuType()) === 'project', [props.menuType, getMenuType]);
  const descriptionEditRef = useRef(false);
  const copingStrategyEditRef = useRef(false);
  const { isShowFeature, loading } = useIsInProgram({ projectId: props.projectId });
  const { isProgram, isAgileProgram } = useIsProgram();
  const value = {
    ...props,
    isProjectLevel,
    menuType: props.menuType || getMenuType(), /** project organization */
    prefixCls: 'c7n-agile-EditIssue',
    intlPrefix: 'agile.EditIssue',
    store: useMemo(() => new EditIssueStore({ projectId: props.projectId || getProjectId() }), [props.projectId]), // 防止update时创建多次store
    FieldVersionRef,
    FieldFixVersionRef,
    descriptionEditRef,
    copingStrategyEditRef,
    saveFieldVersionRef: (ref) => {
      FieldVersionRef.current = ref;
    },
    saveFieldFixVersionRef: (ref) => {
      FieldFixVersionRef.current = ref;
    },
    isShowFeature,
    isProgram,
    isAgileProgram,
  };
  const { isProgramIssueType: isProgramIssue } = useIsProgramIssueType({ typeCode: value.store.issue.typeCode, applyType: value.store.issue?.applyType });
  useEffect(() => {
    value.store.initApi(props.outside, props.organizationId, props.projectId || getProjectId());
    return () => {
      value.store.destroy();
    };
  }, [props.organizationId, props.outside, props.projectId, value.store]);

  useEffect(() => {
    value.store.setTab(props.tab);
  }, [props.tab, value.store]);
  return (
    <EditIssueContext.Provider value={{ ...value, isProgramIssue }}>
      {!loading && props.children}
    </EditIssueContext.Provider>
  );
}));
