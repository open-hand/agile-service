import React, { useMemo, forwardRef } from 'react';
import { Select, Tooltip } from 'choerodon-ui/pro';
import { versionApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IVersion } from '@/common/types';
import { getProjectId } from '@/utils/common';
import FlatSelect from '@/components/flat-select';

interface Props extends Partial<SelectProps> {
  projectId?: string
  dataRef?: React.MutableRefObject<Array<any>>
  statusArr?: Array<string>
  valueField?: string
  afterLoad?: (versions: IVersion[]) => void
  request?: Function
  flat?:boolean
  hasUnassign?: boolean
}
const SelectVersion: React.FC<Props> = forwardRef(({
  request, projectId, valueField, dataRef = { current: null }, afterLoad, statusArr = [], flat, hasUnassign, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<IVersion> => ({
    name: 'version',
    textField: 'name',
    valueField: valueField || 'name',
    request: () => {
      if (request) {
        return request();
      }
      return versionApi.project(projectId || getProjectId()).loadNamesByStatus(statusArr);
    },
    middleWare: (versions: IVersion[]) => {
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
    optionRenderer: !flat ? (c) => (
      <Tooltip title={c.name} placement="left">
        {c.name}
      </Tooltip>
    ) : undefined,
    paging: false,
  }), [projectId]);
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
