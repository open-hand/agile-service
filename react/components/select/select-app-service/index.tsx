import React, { useMemo, forwardRef } from 'react';
import { Select, Tooltip } from 'choerodon-ui/pro';
import useSelect, { SelectConfig, FragmentForSearch } from '@/hooks/useSelect';
import { devOpsApi, versionApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { ILabel } from '@/common/types';
import FlatSelect from '@/components/flat-select';

interface Props extends Partial<SelectProps> {
  dataRef?: React.RefObject<Array<any>>
  valueField?: string
  afterLoad?: (list: any[]) => void
  flat?: boolean
  programMode?: string /** 是否为项目群访问模式  */
  projectId?: string
}
const renderService = (appService: any) => {
  if (appService) {
    return <Tooltip title={appService.code}>{`${appService.name}(${appService.code})`}</Tooltip>;
  }
  return null;
};
const SelectAppService: React.FC<Props> = forwardRef(({
  dataRef, valueField, afterLoad, flat, projectId, programMode, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig => ({
    name: 'appService',
    textField: 'name',
    valueField: valueField || 'code',
    optionRenderer: (appService: any) => (
      <FragmentForSearch name={`${appService.name}(${appService.code})`}>
        {renderService(appService)}
      </FragmentForSearch>
    ),
    request: () => devOpsApi.loadActiveService(),
    middleWare: (data: any) => {
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
  }), []);
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
export default SelectAppService;
