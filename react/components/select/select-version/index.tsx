import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { versionApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IVersion } from '@/common/types';

interface Props extends Partial<SelectProps> {
  dataRef?: React.MutableRefObject<Array<any>>
  statusArr?: Array<string>
  valueField?: string
  afterLoad?: (versions: IVersion[]) => void
}
const SelectVersion: React.FC<Props> = forwardRef(({
  valueField, dataRef = { current: null }, afterLoad, statusArr = [], ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig => ({
    name: 'version',
    textField: 'name',
    valueField: valueField || 'name',
    request: () => versionApi.loadNamesByStatus(statusArr),
    middleWare: (versions: IVersion[]) => {
      if (dataRef) {
        Object.assign(dataRef, {
          current: versions,
        });
      }
      if (afterLoad) {
        afterLoad(versions);
      }
      return versions;
    },
    paging: false,
  }), []);
  const props = useSelect(config);
  return (
    <Select
      multiple
      ref={ref}
      clearButton
      {...props}
      {...otherProps}
    />
  );
});
export default SelectVersion;
