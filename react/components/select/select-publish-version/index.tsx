import React, { useMemo, forwardRef } from 'react';
import { Select, Tooltip } from 'choerodon-ui/pro';
import { IAppVersionData, publishVersionApi, versionApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IVersion } from '@/common/types';
import { getProjectId } from '@/utils/common';
import { FlatSelect } from '@choerodon/components';

interface Props extends Partial<SelectProps> {
  projectId?: string
  dataRef?: React.MutableRefObject<Array<any>>
  statusArr?: Array<string>
  valueField?: string
  afterLoad?: (versions: IAppVersionData[]) => (void | any[])
  request?: Function
  flat?: boolean
  hasUnassign?: boolean
}
const SelectPublishVersion: React.FC<Props> = forwardRef(({
  request, projectId, valueField, dataRef = { current: null }, afterLoad, statusArr = [], flat, hasUnassign, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<IAppVersionData> => ({
    name: 'publishVersion',
    textField: 'versionAlias',
    valueField: valueField || 'id',
    request: ({ filter, page }) => {
      if (request) {
        return request();
      }
      return publishVersionApi.project(projectId || getProjectId()).loadList({ page: page!, size: 20 }, { appService: true, content: filter });
    },
    middleWare: (versions: IAppVersionData[]) => {
      let newVersion = versions;

      if (dataRef) {
        Object.assign(dataRef, {
          current: newVersion,
        });
      }
      if (afterLoad) {
        const temp = afterLoad(versions);
        newVersion = Array.isArray(temp) ? temp : newVersion;
      }
      if (hasUnassign) {
        // newVersion = [{ versionId: '0', name: '未分配版本' } as IVersion, ...versions];
      }
      return newVersion;
    },
    tooltip: true,
    paging: true,
  }), [projectId]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;

  return (
    <Component
      // multiple
      ref={ref}
      clearButton
      {...props}
      {...otherProps}
    />
  );
});
export default SelectPublishVersion;
