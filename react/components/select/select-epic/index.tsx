import React, { useMemo, forwardRef, useRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { toJS } from 'mobx';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import { castArray, uniq } from 'lodash';
import { useComputed } from 'mobx-react-lite';
import { epicApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { IEpic } from '@/components/charts/epic-report/search';
import { refsBindRef } from '../utils';
import { useRefsBindRef } from '@/hooks/useRefsBindRef';
import useSelectRequestArgsValue from '../useSelectRequestArgsValue';

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
  isProgram, afterLoad, dataRef: propsDataRef, dontAddEpic0, unassignedEpic, request, flat, projectId, defaultSelectedIds, onlyUnCompleted = true, selectIds: propsSelectIds, ...otherProps
}, ref: React.Ref<Select>) => {
  const innerDataRef = useRef<any>();
  const selectRef = useRef<Select>();
  const values = useComputed(() => (selectRef.current?.getValues() || []).flat(Infinity).map((item) => (typeof item === 'object' ? item.issueId : item)), [selectRef.current?.getValues()]);
  const selectIds = useMemo(() => uniq([...castArray(toJS(propsSelectIds)), ...castArray(toJS(defaultSelectedIds)), ...values].filter(Boolean)), [propsSelectIds, values]);
  const selected = useSelectRequestArgsValue({ dataRef: innerDataRef, value: selectIds });
  const args = useMemo(() => ({ selectIds: selected }), [selected]);
  const dataRef = useRefsBindRef(propsDataRef, innerDataRef);

  const config = useMemo((): SelectConfig => ({
    name: 'epic',
    textField: 'epicName',
    valueField: 'issueId',
    requestArgs: args,
    request: request || (({ page, filter, requestArgs }) => (isProgram ? epicApi.project(projectId).loadProgramEpics({
      page,
      onlyUnCompleted,
      param: filter,
    }, requestArgs?.selectIds) : epicApi.project(projectId).loadEpicsForSelect(projectId, {
      page,
      onlyUnCompleted,
      param: filter,
    }, requestArgs?.selectIds))),
    middleWare: (epicList: IEpic[]) => {
      const temp = [...epicList];
      if (unassignedEpic || (isProgram && !dontAddEpic0)) {
        temp.unshift({ issueId: '0', epicName: '未分配史诗' });
      }
      if (afterLoad) {
        afterLoad(temp);
      }
      return temp;
    },
    dataRef,
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
