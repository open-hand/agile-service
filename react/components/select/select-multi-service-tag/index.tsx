import React, {
  useMemo, forwardRef, useEffect, useRef, useCallback, useState,
} from 'react';
import {
  Select, DataSet, Form, Progress, Button, Tooltip,
} from 'choerodon-ui/pro';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import {
  observer, useComputed, useForceUpdate, useObservable, useObserver,
} from 'mobx-react-lite';
import { toJS } from 'mobx';
import { groupBy } from 'lodash';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { devOpsApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { ILabel } from '@/common/types';
import { FlatSelect } from '@choerodon/components';
import { getProjectId } from '@/utils/common';
import { randomString } from '@/utils/random';
import SelectGitTags from '../select-git-tags';
import './index.less';

const { Option } = Select;
const prefixCls = 'c7n-agile-select-multi-service-tag';
const renderService = (appService: any) => {
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
const SelectAppService: React.FC<Partial<SelectProps> & { loadData: (record: Record) => any[] }> = observer(({ record, loadData, ...otherProps }) => {
  // const options = useObserver(() => (loadData(record!) as any[]).map((item) => <Option value={item.id}>{renderService(item)}</Option>));
  const optionData = useComputed(() => (loadData(record!) as any[]).map((item) => ({ ...item, meaning: `${item.name}(${item.code})`, value: item.code })), []);
  const optionsDs = useMemo(() => new DataSet({
    data: optionData,
    paging: false,
    autoCreate: false,
    autoQuery: false,
  }), [optionData]);
  return (
    <Select record={record} {...otherProps} options={optionsDs} />
  );
});
interface Props extends Partial<SelectProps> {
  dataRef?: React.RefObject<Array<any>>
  valueField?: string
  afterLoad?: (sprints: ILabel[]) => void
  onChange?: (data: ITagItemProps[]) => void
  onBlur?: () => void
  applicationId?: string | null
  flat?: boolean
  projectId?: string
}
interface ITagItemProps {
  tagName: string
  appServiceCode: string
  projectId: string

}
interface IMultiServiceTagProps {
  onOK?: (data: ITagItemProps[]) => void
  onCancel?: () => void
  data: Array<Pick<ITagItemProps, 'appServiceCode'> & { tagName: string[] }>
  projectId?: string
}
const MultiServiceTag: React.FC<IMultiServiceTagProps> = observer(({
  onOK, onCancel, projectId, data,
}) => {
  const appServiceList = useObservable([] as any[]);
  // const [appServiceList, setAppServiceList] = useState([] as any[]);
  const ds = useMemo(() => new DataSet({
    autoCreate: false,
    autoQuery: false,
    paging: false,
    data,
    fields: [
      {
        name: 'appService', label: '应用服务', textField: 'meaning', valueField: 'code', type: 'object' as any, ignore: 'always' as any,
      },
      { name: 'appServiceId', label: '应用服务', bind: 'appService.id' },
      { name: 'appServiceCode', label: '应用服务', bind: 'appService.code' },
      { name: 'tagName', label: 'Tag' },
    ],
  }), [data]);
  useEffect(() => {
    function loadAppServiceData() {
      devOpsApi.project(projectId).loadActiveService().then((res: any) => {
        Array.isArray(res) && appServiceList.push(...res);
        // setAppServiceList(res);
      });
    }
    loadAppServiceData();
  }, [projectId]);
  useEffect(() => {
    if (appServiceList.length > 0 && !ds.getState('data_int')) {
      ds.records.forEach((record) => {
        const appServiceCode = record.get('appServiceCode');
        const appServiceId = record.get('appServiceId');
        if (appServiceCode && !appServiceId) {
          const findItem = appServiceList.find((item) => item.code === appServiceCode);
          record.init('appService', findItem ? { ...findItem, meaning: `${findItem.name}(${findItem.code})` } : { appServiceCode, meaning: appServiceCode, value: appServiceCode });
        }
      });
      ds.setState('data_int', true);
    }
  }, [appServiceList, appServiceList.length, ds]);
  const handleCreate = useCallback(() => {
    ds.create();
  }, [ds]);
  function handleCancel(e: any) {
    onCancel && onCancel();
    ds.reset();
    e.stopPropagation();
  }
  function handleSave(e: any) {
    const saveData: ITagItemProps[] = [];
    ds.toJSONData().forEach((item: any) => {
      saveData.push(...(item.tagName || []).map((tag: string) => ({ tagName: tag, appServiceCode: item.appServiceCode, projectId: projectId || getProjectId() })));
    });
    onOK && onOK(saveData);
    handleCancel(e);
  }

  useEffect(() => {
    // 若无数据则创建
    data && data.length === 0 && handleCreate();
  }, [data, handleCreate]);

  const loadAppServiceData = useCallback((record: Record) => {
    const applicationCodes = ds.map((r) => r.get('appServiceCode'));
    const recordAppServiceList = appServiceList.filter((appService) => record.get('appServiceCode') === appService.code || !applicationCodes.includes(appService.code));
    return recordAppServiceList;
  }, [appServiceList, ds]);
  const componentId = useMemo(() => `select-multi-service-tag-${randomString(5)}`, []);

  return (
    <div className={prefixCls}>
      <div role="none" id={componentId} className={`${prefixCls}-content`} onClick={(e) => e.stopPropagation()} onMouseDown={(e) => e.stopPropagation()}>
        <Form dataSet={ds}>
          {ds.records.map((record) => {
            const applicationId = record.get('applicationId') || appServiceList.find((item) => item.code === record.get('appServiceCode'))?.id;
            return [<SelectAppService
              name="appService"
              record={record}
              loadData={loadAppServiceData}
              onChange={() => record.init('tagName', undefined)}
              style={{ marginBottom: '-.06rem', width: '90%' }}
              getPopupContainer={() => document.getElementById(componentId) as any}
            />,
              <div className={`${prefixCls}-content-del-wrap`}>
                <SelectGitTags
                  applicationId={applicationId}
                  name="tagName"
                  multiple
                  style={{ width: '90%' }}
                  record={record}
                  getPopupContainer={() => document.getElementById(componentId) as any}
                />
                {record.index !== 0 && <Button icon="delete_forever" className={`${prefixCls}-content-del`} onClick={() => { ds.delete(record, false); }} />}
              </div>];
          })}
          {ds.length > 0 && <Button className={`${prefixCls}-content-add`} icon="add" funcType={'flat' as any} color={'primary' as any} onClick={handleCreate}>添加Tag</Button>}
        </Form>

      </div>
      <div className={`${prefixCls}-footer`}>
        <Button funcType={'flat' as any} color={'primary' as any} onClick={handleSave}>确定</Button>
        <Button funcType={'flat' as any} onClick={handleCancel}>取消</Button>

      </div>
    </div>

  );
});
const SelectMultiServiceTag: React.FC<Props> = forwardRef(({
  dataRef, valueField, afterLoad, flat, applicationId, projectId, onBlur, onChange, value: propsValue, ...otherProps
}, ref: React.Ref<Select>) => {
  const innerRef = useRef<Select | null>();
  const handleBindRef = useCallback((newRef) => {
    if (newRef) {
      ref && Object.assign(ref, { current: newRef });
      Object.assign(innerRef, {
        current: Object.assign(newRef, {
          focus: () => {
            innerRef.current?.trigger?.delaySetPopupHidden(false, 150);
          },
        }),
      });
    }
  }, [ref]);

  function handleCancel() {
    innerRef.current?.collapse();
    onBlur && onBlur();
  }
  function handleSave(data: ITagItemProps[]) {
    onChange && onChange(data);
    handleCancel();
  }
  const value = useMemo(() => toJS(propsValue)?.map((item: any) => ({ meaning: (item.appServiceCode ? `${item.appServiceCode}:${item.tagName}` : item.tagName), value: item })), [propsValue]);
  const editValue = useMemo(() => {
    // 合并相同code的tag选项
    if (value && value.length > 0) {
      const groupObj = groupBy(value, (v) => v.value.appServiceCode);
      return Object.entries(groupObj).map(([code, tags]) => ({ appServiceCode: code, tagName: tags?.map((v) => v.value.tagName) }));
    }
    return [];
  }, [value]);

  const Component = flat ? FlatSelect : Select;
  const componentId = useMemo(() => `select-multi-service-tag-${randomString(5)}`, []);

  return (
    <div id={componentId}>
      <Component
        ref={handleBindRef}
        value={value}
        primitiveValue={false}
        onPopupHiddenChange={(hidden) => {
          console.log('blur..', hidden, onBlur);
          // onChange();

          hidden && onBlur && onBlur();
        }}
        getPopupContainer={(node) => document.getElementById(componentId) as HTMLElement}
        trigger={['click'] as any}
        onChange={(v) => {
          let newValue = v;
          if (v && v.length > 0) {
            newValue = v.map((i: any) => i.value);
          }
          onChange && onChange(newValue);
          console.log('onChange', v);
        }}
        {...otherProps}
        dropdownMatchSelectWidth={false}
        popupContent={editValue ? <MultiServiceTag onOK={handleSave} data={editValue} onCancel={handleCancel} projectId={projectId} /> : <div />}
      />
    </div>
  );
});

export default SelectMultiServiceTag;
