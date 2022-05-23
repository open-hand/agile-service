import React, {
  createContext, useCallback, useEffect, useMemo, useRef,
} from 'react';
import { inject } from 'mobx-react';
import { observer } from 'mobx-react-lite';
import EditIssueStore from './EditIssueStore';
import { getMenuType, getProjectId } from '@/utils/common';
import useIsInProgram from '@/hooks/useIsInProgram';
import useIsProgram from '@/hooks/useIsProgram';
import useIsProgramIssueType from '@/hooks/useIsProgramIssueType';
import useIsWaterfall from '@/hooks/useIsWaterfall';

const EditIssueContext = createContext();
export default EditIssueContext;
export const EditIssueContextProvider = inject('AppState', 'HeaderStore')(observer((props) => {
  const FieldVersionRef = {
    current: null,
  };
  const FieldFixVersionRef = {
    current: null,
  };
  const { isWaterfall } = useIsWaterfall();
  const isProjectLevel = useMemo(() => (props.menuType || getMenuType()) === 'project', [props.menuType, getMenuType]);
  const descriptionEditRef = useRef(false);
  const copingStrategyEditRef = useRef(false);
  const store = useMemo(() => new EditIssueStore({ projectId: props.projectId || getProjectId() }), [props.projectId]);
  // 防止update时创建多次store
  const { isShowFeature, loading } = useIsInProgram({ projectId: props.projectId, categories: store.issueProjectCategories });
  const { isProgram, isAgileProgram } = useIsProgram();
  const { isProgramIssueType: isProgramIssue } = useIsProgramIssueType({ typeCode: store.issue.typeCode, applyType: store.issue?.applyType });

  const getDeletePermissions = useCallback(() => {
    if (!store.issue.issueId) {
      return [];
    }
    const { typeCode } = store.issue || {};
    if (typeCode === 'risk') {
      return ['choerodon.code.project.cooperation.risk.delete'];
    }
    if (isWaterfall) {
      return ['choerodon.code.project.cooperation.sprint.iteration-plan.ps.editissue.pro'];
    }
    if (isProgramIssue) {
      return ['choerodon.code.project.plan.feature.ps.choerodon.code.project.plan.feature.editissue.pro'];
    }
    return ['choerodon.code.project.cooperation.iteration-plan.ps.choerodon.code.agile.project.editissue.pro'];
  }, [isProgramIssue, isWaterfall, store.issue]);
  const value = {
    ...props,
    applyType: props.applyType ?? 'agile',
    isProjectLevel,
    menuType: props.menuType || getMenuType(), /** project organization */
    prefixCls: 'c7n-agile-EditIssue',
    intlPrefix: 'agile.EditIssue',
    store,
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
    getDeletePermissions,
  };
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
