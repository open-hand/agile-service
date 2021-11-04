import React, { useMemo, forwardRef, useRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { toJS } from 'mobx';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import { castArray } from 'lodash';
import { useComputed } from 'mobx-react-lite';
import { epicApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { IEpic } from '@/components/charts/epic-report/search';
import { useNoticeSelectUpdateSelected } from '../useNoticeSelectUpdateSelected';
import { refsBindRef, wrapRequestCallback } from '../utils';

export interface SelectEpicProps extends Partial<SelectProps> {
  isProgram?: boolean
  request?: SelectConfig<any>['request']
  dataRef?: React.MutableRefObject<any>
  afterLoad?: (epics: IEpic[]) => void
  dontAddEpic0?: boolean
  unassignedEpic?: boolean
  /** 仅展示已完成史诗 @default true  自定义request 本配置无效  */
  onlyUnCompleted?: boolean
  /** 默认已选中的 */
  defaultSelectedIds?: string | string[]
  /** 受控值 已选中 会自动加载 */
  selectIds?: string | string[]
  flat?: boolean
  projectId?: string
}

const SelectEpic: React.FC<SelectEpicProps> = forwardRef(({
  isProgram, afterLoad, dataRef, dontAddEpic0, unassignedEpic, request, flat, projectId, defaultSelectedIds, onlyUnCompleted = true, selectIds: propsSelectIds, ...otherProps
}, ref: React.Ref<Select>) => {
  const selectRef = useRef<Select>();
  const values = useComputed(() => (selectRef.current?.getValues() || []).map((item) => (typeof item === 'object' ? item.issueId : item)), [selectRef.current?.getValues()]);

  const selectIds = useMemo(() => [...castArray(toJS(propsSelectIds)), ...castArray(toJS(defaultSelectedIds)), ...values].filter(Boolean), [propsSelectIds, values]);
  const selectIdsRef = useRef<string[]>(selectIds);

  const [forceValue, setFilterWord] = useNoticeSelectUpdateSelected();
  const optionsRef = useRef<any[]>();
  const args = useMemo(() => {
    if (optionsRef.current && selectIds) {
      // 有新的未加载的值，就重新加载
      const hasNewUnExistValue = selectIds.some((v) => !optionsRef.current?.find((item) => item.issueId === v));
      if (hasNewUnExistValue || forceValue) {
        selectIdsRef.current = selectIds;
      }
      selectIdsRef.current = selectIds;
    }
    return { selectIds: selectIdsRef.current };
  }, [forceValue, selectIds]);
  const config = useMemo((): SelectConfig => ({
    name: 'epic',
    textField: 'epicName',
    valueField: 'issueId',
    requestArgs: args,
    request: wrapRequestCallback(request || (({ page, filter, requestArgs }) => (isProgram ? epicApi.project(projectId).loadProgramEpics({
      page,
      onlyUnCompleted,
      param: filter,
    }, requestArgs?.selectIds) : epicApi.project(projectId).loadEpicsForSelect(projectId, {
      page,
      onlyUnCompleted,
      param: filter,
    }, requestArgs?.selectIds))), ({ filter }) => setFilterWord('filter', filter)),
    middleWare: (epicList: IEpic[]) => {
      const temp = [...epicList];
      if (unassignedEpic || (isProgram && !dontAddEpic0)) {
        temp.unshift({ issueId: '0', epicName: '未分配史诗' });
      }
      if (dataRef) {
        Object.assign(dataRef, {
          current: (unassignedEpic || (isProgram && !dontAddEpic0)) ? temp.unshift({ issueId: '0', epicName: '未分配史诗' }) : temp,
        });
      }
      if (afterLoad) {
        afterLoad(temp);
      }
      optionsRef.current = temp;
      return temp;
    },
    paging: true,
  }), [afterLoad, args, dataRef, dontAddEpic0, isProgram, onlyUnCompleted, projectId, request, unassignedEpic]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;

  return (
    <Component
      ref={refsBindRef(ref, selectRef)}
      clearButton
      {...props}
      {...otherProps}
    />
  );
});
export default SelectEpic;
