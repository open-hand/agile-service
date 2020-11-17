import React, { useMemo, forwardRef } from 'react';
import { Select, Tooltip } from 'choerodon-ui/pro';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { versionApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IProgramVersion } from '@/common/types';
import FlatSelect from '@/components/flat-select';

interface Props extends Partial<SelectProps> {
  teamProjectIds?: string[],
  dataRef?: React.MutableRefObject<any>
  afterLoad?: (versions: any[]) => void
  flat?: boolean
}

const SelectProgramVersion: React.FC<Props> = forwardRef(({
  teamProjectIds, dataRef, afterLoad, flat, ...otherProps
},
ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<IProgramVersion> => ({
    name: 'program_version',
    textField: 'name',
    valueField: 'id',
    request: () => versionApi.loadProgramVersion(false, teamProjectIds),
    optionRenderer: (item) => <Tooltip>{item.name}</Tooltip>,
    middleWare: (data: IProgramVersion[]) => {
      if (dataRef) {
        Object.assign(dataRef, {
          current: data,
        });
      }
      if (afterLoad) {
        afterLoad(data);
      }
      return data;
    },
    paging: false,
  }), [afterLoad, dataRef, teamProjectIds]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;
  return (
    <Component
      ref={ref}
      clearButton={false}
      {...props}
      {...otherProps}
    />
  );
});
export default SelectProgramVersion;
