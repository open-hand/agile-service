import React, { useMemo, forwardRef } from 'react';
import { Select, Tooltip, DataSet } from 'choerodon-ui/pro';
import { omit } from 'lodash';
import classnames from 'classnames';
import useSelect, { SelectConfig, FragmentForSearch } from '@/hooks/useSelect';
import { devOpsApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import styles from './index.less';

const { OptGroup, Option } = Select;
interface Props extends Partial<SelectProps> {
  dataRef?: React.RefObject<Array<any>>
  valueField?: string
  afterLoad?: (list: any[]) => void
  flat?: boolean
  request?: SelectConfig['request'],
  mode?: 'other' | 'self'/** @default 'self' self 本项目模式 other 其他项目模式 */
  projectId?: string
  checkMember?: boolean
}
const renderService = (appService: any) => {
  if (appService) {
    return (
      <Tooltip title={appService.code}>
        <div style={{ display: 'inline-block', zIndex: 99999 }}>
          {`${appService.name}(${appService.code})`}
        </div>
      </Tooltip>
    );
  }
  return null;
};
const SelectSelfAppService: React.FC<Props> = forwardRef(({
  dataRef, valueField, afterLoad, flat, projectId, request, className, checkMember, ...otherProps
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
    request: request || (() => devOpsApi.project(projectId).loadActiveService(checkMember)),
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
  }), [checkMember, projectId]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;

  return (
    <Component
      ref={ref}
      {...props}
      {...otherProps}
      className={classnames(className, styles.wrap)}

    />
  );
});
const SelectTestOtherAppService: React.FC<Props> = forwardRef(({
  dataRef, valueField, afterLoad, flat, projectId, request, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig => ({
    name: 'appService',
    textField: 'projectName',
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
      help="此处会展示出每个项目下最多5个应用服务，若想选择其他应用服务，需要手动输入服务名进行搜索"
      showHelp={'tooltip' as any}
    >
      {props.options.map((record) => {
        const optionProps = props.onOption({ record });
        if (record.get('loadMoreButton')) {
          return <Option {...optionProps}>{props.optionRenderer({ record } as any)}</Option>;
        }
        return (
          <OptGroup key={`OptGroup%${record.get('projectId')}`} label={record.get('meaning')}>
            {record.get('appServices')?.map((i: any) => <Option value={{ projectId: record.get('projectId'), id: i.id }} {...optionProps}>{renderService(i)}</Option>)}
          </OptGroup>
        );
      })}
    </Component>
  );
});
const SelectAppService: React.FC<Props> = forwardRef(({
  mode = 'self', ...otherProps
}, ref: React.Ref<Select>) => React.cloneElement(mode === 'self' ? <SelectSelfAppService /> : <SelectTestOtherAppService />, {
  ref,
  ...otherProps,
}));
export default SelectAppService;
