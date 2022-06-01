import React, { useMemo, forwardRef, useRef } from 'react';
import { Select, Tooltip, DataSet } from 'choerodon-ui/pro';
import { isEmpty, omit } from 'lodash';
import classnames from 'classnames';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import { useComputed } from 'mobx-react-lite';
import { useCreation } from 'ahooks';
import useSelect, { SelectConfig, FragmentForSearch } from '@/hooks/useSelect';
import { devOpsApi } from '@/api';
import styles from './index.less';
import { refsBindRef } from '../utils';
import useSelectRequestArgsValue from '../useSelectRequestArgsValue';
import { useRefsBindRef } from '@/hooks/useRefsBindRef';

const { OptGroup, Option } = Select;
interface Props extends Partial<SelectProps> {
  dataRef?: React.RefObject<Array<any>>
  /** 分页模式下不适用 */
  valueField?: string
  afterLoad?: (list: any[]) => void
  flat?: boolean
  request?: SelectConfig['request'],
  mode?: 'other' | 'self' | 'page'/** @default 'self' self 本项目模式 other 其他项目模式  page 分页模式的查询 */
  /** 分页模式下查询目标项目下的服务 */
  pageTargetProjectId?: string
  projectId?: string
  checkMember?: boolean
}
type SelectAppServiceProps = Omit<Props, 'mode' | 'pageTargetProjectId'> & Exclude<Pick<Props, 'mode'>, 'page'> | (Omit<Props, 'mode' | 'pageTargetProjectId'> & Required<Pick<Props, 'pageTargetProjectId'>> & { mode: 'page' })

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
const SelectPageAppService: React.FC<Props> = forwardRef(({
  dataRef: propsDataRef, valueField, afterLoad, flat, projectId, request, className, pageTargetProjectId, checkMember, ...otherProps
}, ref: React.Ref<Select>) => {
  const innerDataRef = useRef<any>();
  const selectRef = useRef<Select>();
  const value = useComputed(() => selectRef.current?.getValue(), [selectRef.current]);
  const targetAppServiceId = useSelectRequestArgsValue({ dataRef: innerDataRef, value });
  const args = useCreation(() => ({ pageTargetProjectId, targetAppServiceId }), [pageTargetProjectId, targetAppServiceId]);
  const dataRef = useRefsBindRef(innerDataRef, propsDataRef);
  const config = useMemo((): SelectConfig => ({
    name: 'appService',
    textField: 'name',
    valueField: 'id',
    requestArgs: args,
    optionRenderer: (appService: any) => (
      <FragmentForSearch name={`${appService.name}(${appService.code})`}>
        {renderService(appService)}
      </FragmentForSearch>
    ),
    request: ({ page, filter, requestArgs }) => (requestArgs?.pageTargetProjectId! ? devOpsApi.project(projectId).loadPageActiveService({
      page,
      size: 10,
      param: filter,
      targetProjectId: requestArgs?.pageTargetProjectId!,
      targetAppServiceId: requestArgs?.targetAppServiceId,
    }) : new Promise((resolve) => resolve({ list: [], hasNextPage: false }))),
    dataRef,
    afterLoad,
    paging: true,
  }), [args, projectId, dataRef, afterLoad]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;

  return (
    <Component
      ref={refsBindRef(ref, selectRef)}
      {...props}
      {...otherProps}
      className={classnames(className, styles.wrap)}

    />
  );
});

const SelectAppService: React.FC<SelectAppServiceProps> = forwardRef(({
  mode = 'self', ...otherProps
}, ref: React.Ref<Select>) => {
  let Component: any;
  switch (mode) {
    case 'self': {
      Component = SelectSelfAppService;
      break;
    }
    case 'other': {
      Component = SelectTestOtherAppService;
      break;
    }
    case 'page': {
      Component = SelectPageAppService;
      break;
    }
    default: {
      Component = Select;
      break;
    }
  }
  return <Component ref={ref} {...otherProps} />;
});
export default SelectAppService;
