import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { versionApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';

interface Props {
  dataRef: React.MutableRefObject<Array<any>>
  statusArr: Array<string>
}
const SelectVersion: React.FC<Props> = forwardRef(({ dataRef = { current: null }, statusArr = [], ...otherProps }, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig => ({
    name: 'version',
    textField: 'name',
    valueField: 'name',
    request: () => versionApi.loadNamesByStatus(statusArr),
    middleWare: (versions) => {
      // eslint-disable-next-line no-param-reassign
      dataRef.current = versions;

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
