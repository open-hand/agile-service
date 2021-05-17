import React, { useMemo, forwardRef } from 'react';
import { Select, Tooltip, DataSet } from 'choerodon-ui/pro';
import { omit } from 'lodash';
import useSelect, { SelectConfig, FragmentForSearch } from '@/hooks/useSelect';
import { devOpsApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';

const { OptGroup, Option } = Select;
interface Props extends Partial<SelectProps> {
  dataRef?: React.RefObject<Array<any>>
  valueField?: string
  afterLoad?: (list: any[]) => void
  flat?: boolean
  request?: SelectConfig['request'],
  mode?: 'other' | 'self'/** @default 'self' */
  projectId?: string
}
const renderService = (appService: any) => {
  console.log('appService.', appService);
  if (appService) {
    return (
      <Tooltip title={appService.code}>
        <div style={{ display: 'inline-block' }}>
          {`${appService.name}(${appService.code})`}
        </div>
      </Tooltip>
    );
  }
  return null;
};
const SelectAppService: React.FC<Props> = forwardRef(({
  dataRef, valueField, afterLoad, flat, projectId, request, ...otherProps
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
    request: request || (() => devOpsApi.project(projectId).loadActiveService()),
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
const SelectTestOtherAppService: React.FC<Props> = forwardRef(({
  dataRef, valueField, afterLoad, flat, projectId, request, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig => ({
    name: 'appService',
    textField: 'projectName',
    optionRenderer: (appService: any) => {
      console.log('appService..optionRenderer', appService);
      return (
        <FragmentForSearch name={`${appService.name}(${appService.code})`}>
          {appService.meaning}
        </FragmentForSearch>
      );
    },
    valueField: valueField || 'projectId',
    request: request || (({ page, filter }) => devOpsApi.project(projectId).loadProjectActiveService(page!, 10, filter)),
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
    paging: true,
  }), []);
  const Component = flat ? FlatSelect : Select;
  const props = useSelect(config);
  const processProps = omit(props, ['options', 'onOption', 'optionRenderer']);
  return (
    <Component
      ref={ref}
      {...processProps}
      {...otherProps}
    >
      {props.options.map((record) => {
        const optionProps = props.onOption({ record });
        if (record.get('loadMoreButton')) {
          return <Option {...optionProps}>{props.optionRenderer({ record } as any)}</Option>;
        }
        return (
          <OptGroup key={`OptGroup-${record.id}`} label={record.get('meaning')}>
            {record.get('appServices')?.map((i: any) => <Option value={i.id} {...optionProps}>{renderService(i)}</Option>)}
          </OptGroup>
        );
      })}
    </Component>
  );
});
export default SelectAppService;
