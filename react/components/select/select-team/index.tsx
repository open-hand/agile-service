import React, { useMemo, forwardRef, useRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import { noop } from 'lodash';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { commonApi, fieldApi } from '@/api';
import { wrapRequestCallback } from '../utils';

export interface SelectTeamProps extends Partial<SelectProps> {
  projectDataRef?: React.RefObject<Array<any>>,
  afterLoad?: (projects: any) => void
  /** 首次请求结束后 */
  afterFirstRequest?: SelectConfig['afterLoad'],
  noAssign?: boolean
  flat?: boolean
  request?: () => Promise<any>
  textField?: string
  valueField?: string
  projectId?: string
  fieldId?: string
  ruleIds?: string[]
  selected?: string[]
  addClear?: boolean
}

const SelectTeam: React.FC<SelectTeamProps> = forwardRef(({
  request, textField, valueField, fieldId, ruleIds, selected, projectDataRef = { current: null }, afterLoad = noop, afterFirstRequest, flat, projectId, noAssign, addClear = false, ...otherProps
}, ref: React.Ref<Select>) => {
  const args = useMemo(() => ({ ruleIds, selected }), [ruleIds, selected]);
  const hasRule = !!args.ruleIds?.length;
  const afterLoadRef = useRef<Function>();
  // useSelect 内的 afterLoad 在 middleWare 前调用的，这里先包裹一层处理
  const afterIdStringLoad = (data: any) => {
    afterLoad(data?.map((item: any) => ({ ...item, projectId: item.projectId?.toString() })));
  };
  afterLoadRef.current = afterIdStringLoad;

  const config = useMemo((): SelectConfig => ({
    name: 'team',
    textField: textField || 'projName',
    valueField: valueField || 'projectId',
    requestArgs: args,
    request: wrapRequestCallback(({ requestArgs, page, filter }) => {
      if (!request) {
        if (hasRule && fieldId) {
          return fieldApi.project(projectId).getCascadeOptions(fieldId, requestArgs?.selected, requestArgs?.ruleIds, filter ?? '', page ?? 0, 0).then((res:any) => ({
            ...res,
            list: res.list?.map((item:any) => ({ ...item, projectId: item.projectId?.toString() })),
          }));
        }
        return commonApi.getSubProjects(true, projectId).then((projects:any) => projects.map((item:any) => ({ ...item, projectId: item.projectId?.toString() })));
      }
      return request();
    }, (_, res) => {
      afterFirstRequest && afterFirstRequest(res);
    }),
    paging: !!(hasRule && fieldId),
    afterLoad: afterLoadRef.current as any,
    middleWare: (projects) => {
      if (Array.isArray(projects)) {
        // @ts-ignore
        const newProjects = projects.map((item) => ({ ...item, projectId: item.projectId?.toString() }));
        // @ts-ignore
        // eslint-disable-next-line
        projectDataRef.current = newProjects;
        const newList = [...(newProjects || [])];
        if (noAssign) {
          // @ts-ignore
          newList.unshift({ projName: '未分配', projectId: '0' });
        }
        if (addClear) {
          // @ts-ignore
          newList.unshift({ projName: '清空', projectId: 'clear' });
        }
        return newList;
      }
      return [];
    },
  }), [addClear, args, fieldId, hasRule, noAssign, projectDataRef, projectId, request, textField, valueField]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;
  return (
    <Component
      ref={ref}
      {...props}
      {...otherProps}
    />
  );
});
export default SelectTeam;
