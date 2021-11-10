import React, { useMemo, forwardRef } from 'react';
import { Select, Tooltip } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import { cloneDeep } from 'lodash';
import { fieldApi, versionApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { IVersion } from '@/common/types';
import { getProjectId } from '@/utils/common';

export interface SelectVersionProps extends Partial<SelectProps> {
  projectId?: string
  dataRef?: React.MutableRefObject<Array<any>>
  statusArr?: Array<string>
  valueField?: string
  afterLoad?: (versions: IVersion[]) => void
  request?: Function
  flat?: boolean
  hasUnassign?: boolean
  ruleIds?: string[]
  selected?: string[]
  fieldId?: string
}
const SelectVersion: React.FC<SelectVersionProps> = forwardRef(({
  request, projectId, valueField, ruleIds, selected, fieldId, dataRef = { current: null }, afterLoad, statusArr = [], flat, hasUnassign, ...otherProps
}, ref: React.Ref<Select>) => {
  const args = useMemo(() => ({ ruleIds, selected }), [ruleIds, selected]);
  const hasRule = Object.keys(args).filter((key: keyof typeof args) => Boolean(args[key])).length > 0;
  const config = useMemo((): SelectConfig<IVersion> => ({
    name: 'version',
    textField: 'name',
    valueField: valueField || 'name',
    requestArgs: args,
    request: ({ requestArgs, filter, page }) => {
      if (request) {
        return request();
      }
      if (hasRule && fieldId) {
        return fieldApi.project(projectId).getCascadeOptions(fieldId, requestArgs?.selected, requestArgs?.ruleIds, filter ?? '', page ?? 0, 0, statusArr);
      }
      return versionApi.project(projectId || getProjectId()).loadNamesByStatus(statusArr);
    },
    middleWare: (versions: IVersion[] = []) => {
      if (dataRef) {
        Object.assign(dataRef, {
          current: versions,
        });
      }
      if (afterLoad) {
        afterLoad(versions);
      }
      let newVersion = versions;
      if (hasUnassign) {
        newVersion = [{ versionId: '0', name: '未分配版本' } as IVersion, ...versions];
      }
      return newVersion;
    },
    tooltip: true,
    paging: !!(hasRule && fieldId),
  }), [afterLoad, args, dataRef, fieldId, hasRule, hasUnassign, projectId, request, statusArr, valueField]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;

  return (
    <Component
      multiple
      ref={ref}
      clearButton
      {...props}
      {...otherProps}
    />
  );
});
export default SelectVersion;
