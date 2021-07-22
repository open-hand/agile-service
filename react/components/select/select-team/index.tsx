import React, { useMemo, forwardRef, useRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { commonApi, fieldApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';

interface Props extends Partial<SelectProps> {
  projectDataRef?: React.RefObject<Array<any>>,
  afterLoad?: (projects: any) => void
  noAssign?: boolean
  flat?: boolean
  request?: () => Promise<any>
  textField?: string
  valueField?: string
  projectId?: string
  fieldId?: string
  ruleIds?: string[]
  selected?: string[]
}

const SelectTeam: React.FC<Props> = forwardRef(({
  request, textField, valueField, fieldId, ruleIds, selected, projectDataRef = { current: null }, afterLoad, flat, projectId, noAssign, ...otherProps
}, ref: React.Ref<Select>) => {
  const args = useMemo(() => ({ ruleIds, selected }), [ruleIds, selected]);
  const hasRule = Object.keys(args).filter((key: keyof typeof args) => Boolean(args[key])).length > 0;
  const afterLoadRef = useRef<Function>();
  afterLoadRef.current = afterLoad;
  const config = useMemo((): SelectConfig => ({
    name: 'team',
    textField: textField || 'projName',
    valueField: valueField || 'projectId',
    requestArgs: args,
    request: ({ requestArgs, page, filter }) => {
      if (!request) {
        if (hasRule && fieldId) {
          return fieldApi.project(projectId).getCascadeOptions(fieldId, requestArgs?.selected, requestArgs?.ruleIds, filter ?? '', page ?? 0, 0);
        }
        return commonApi.getSubProjects(true, projectId);
      }
      return request();
    },
    paging: false,
    // @ts-ignore
    afterLoad: afterLoadRef.current,
    middleWare: (projects) => {
      if (Array.isArray(projects)) {
        // @ts-ignore
        // eslint-disable-next-line
        projectDataRef.current = projects;
        return noAssign ? [{ projName: '未分配', projectId: '0' }, ...projects] : projects || [];
      }
      // @ts-ignore

      // eslint-disable-next-line no-param-reassign
      projectDataRef.current = (projects as any).content || [];
      return (projects as any).content || [];
    },
  }), [args, fieldId, hasRule, noAssign, projectDataRef, projectId, request, textField, valueField]);
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
