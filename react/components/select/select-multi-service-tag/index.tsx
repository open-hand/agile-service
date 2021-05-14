import React, {
  useMemo, forwardRef, useEffect, useRef, useCallback, useState,
} from 'react';
import {
  Select, DataSet, Form, Progress, Button,
} from 'choerodon-ui/pro';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { observer, useForceUpdate } from 'mobx-react-lite';
import { toJS } from 'mobx';
import { groupBy } from 'lodash';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { devOpsApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { ILabel } from '@/common/types';
import { FlatSelect } from '@choerodon/components';
import { getProjectId } from '@/utils/common';
import { randomString } from '@/utils/random';
import SelectAppService from '../select-app-service';
import SelectGitTags from '../select-git-tags';
import './index.less';

const prefixCls = 'c7n-agile-select-multi-service-tag';
interface Props extends Partial<SelectProps> {
  dataRef?: React.RefObject<Array<any>>
  valueField?: string
  afterLoad?: (sprints: ILabel[]) => void
  applicationId?: string | null
  flat?: boolean
  projectId?: string

}
const MultiServiceTag: React.FC<{ onOK: (records: Record[]) => void, projectId?: string, onCancel: () => void, originData?: any[] }> = observer(({
  onOK, onCancel, projectId, originData,
}) => {
  const selectedIdSets = useMemo(() => new Set<string>(), []);
  const serviceOptionDs = useMemo(() => new DataSet({
    primaryKey: 'id',
    paging: false,
  }), []);
  const ds = useMemo(() => new DataSet({
    autoCreate: false,
    autoQuery: false,
    paging: false,
    fields: [
      {
        name: 'appService', label: '应用服务', type: 'object' as any, ignore: 'always' as any,
      },
      { name: 'appServiceId', label: '应用服务', bind: 'appService.id' },
      { name: 'appServiceCode', label: '应用服务', bind: 'appService.code' },

      { name: 'tagName', label: 'Tag', parentField: 'appService' },
    ],
    events: {
      update: ({ value, name, oldValue }: any) => {
        if (name === 'appService') {
          const paramId = value?.id || oldValue?.id;
          console.log('paramId', serviceOptionDs.find((item) => item.get('id') === paramId));
          // value.id && console.log('service record', serviceOptionDs.findRecordById(value.id));
          // value && serviceOptionDs.remove();
          // oldValue && selectedIdSets.delete(oldValue?.id);
        }
      },
    },
  }), []);

  const handleCreate = useCallback(() => {
    ds.create();
  }, [ds]);
  function handleCancel(e: any) {
    onCancel();
    e.stopPropagation();
  }
  function handleSave(e: any) {
    const data = ds.toJSONData();
    onOK(ds.records);
    handleCancel(e);
  }
  useEffect(() => {
    function loadData() {
      devOpsApi.project(projectId).loadActiveService().then((res: any) => {
        const data = res.map((item: any) => ({ ...item, meaning: item.name, value: item.id }));
        serviceOptionDs.loadData(data);
        if (originData && originData.length > 0) {
          ds.loadData(originData.map((item) => {
            const appService = data.find((i: any) => i.code === item.appServiceCode);
            return { ...item, appService };
          }));
        } else {
          ds.create();
        }
      });
    }
    loadData();
  }, [ds, originData, projectId, serviceOptionDs]);
  const componentId = useMemo(() => `select-multi-service-tag-${randomString(5)}`, []);
  return (
    <div className={prefixCls}>
      <div role="none" id={componentId} className={`${prefixCls}-content`} onClick={(e) => e.stopPropagation()} onMouseDown={(e) => e.stopPropagation()}>
        <Form dataSet={ds}>
          {ds.records.flatMap((record) => (
            [<SelectAppService
              name="appService"
              options={serviceOptionDs}
              record={record}
              primitiveValue
              style={{ marginBottom: '-.06rem', width: '90%' }}
              getPopupContainer={() => document.getElementById(componentId) as any}
            />,
              <div className={`${prefixCls}-content-del-wrap`}>
                <SelectGitTags
                  key={`select-tag-${record.get('appServiceId')}`}
                  applicationId={record.get('appServiceId')}
                  name="tagName"
                  multiple
                  style={{ width: '90%' }}
                  record={record}
                  getPopupContainer={() => document.getElementById(componentId) as any}
                />
                {record.index !== 0 && <Button icon="delete_forever" className={`${prefixCls}-content-del`} onClick={() => { ds.delete(record, false); }} />}
              </div>]
          ))}
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
      Object.assign(innerRef, { current: newRef });
    }
  }, [ref]);
  useEffect(() => {
    console.log('Component useEffect into SelectMultiServiceTag');
    return () => console.log('leave Component SelectMultiServiceTag');
  }, []);
  const Component = flat ? FlatSelect : Select;
  function handleCancel() {
    innerRef.current?.collapse();
  }
  function handleSave(data: Record[]) {
    // innerRef.current?.choose(data);
    const changeData: any[] = [];
    data.map((i) => i.toJSONData()).forEach((item) => {
      changeData.push(...item.tagName.map((tag: string) => ({ tagName: tag, appServiceCode: item.appServiceCode, projectId: projectId || getProjectId() })));
    });
    console.log('handleSave...', changeData);
    onChange && onChange(changeData, []);
    handleCancel();
  }
  const value = useMemo(() => {
    console.log('value...', propsValue);
    return toJS(propsValue)?.map((item: any) => ({ meaning: (item.appServiceCode ? `${item.appServiceCode}:${item.tagName}` : item.tagName), value: item }));
  }, [propsValue]);
  const editValue = useMemo(() => {
    console.log('editValue propsValue', propsValue);
    // 合并相同code的tag选项
    if (value && value.length > 0) {
      const groupObj = groupBy(value, (v) => v.value.appServiceCode);
      return Object.entries(groupObj).map(([code, tags]) => ({ appServiceCode: code, tagName: tags?.map((v) => v.value.tagName) }));
    }
    return undefined;
  }, [value]);
  return (
    <Component
      ref={handleBindRef}
      value={value}
      // onBlur={() => {
      //   console.log('blur..');
      //   innerRef.current?.collapse();
      // }}
      primitiveValue={false}
      onPopupHiddenChange={(hidden) => {
        console.log('blur..', onBlur);
        // onChange();
        hidden && onBlur && onBlur({} as any);
        // setMode(dateType === 'datetime' ? 'dateTime' : dateType);
      }}
      // getPopupContainer={(node) => node.parentNode as any}
      trigger={['click'] as any}
      onChange={(v) => {
        let newValue = v;
        if (v && v.length > 0) {
          newValue = v.map((i:any) => i.value);
        }
        onChange && onChange(newValue, []);
        console.log('onChange', v);
      }}
      {...otherProps}
      dropdownMatchSelectWidth={false}
      popupContent={<MultiServiceTag onOK={handleSave} originData={editValue} onCancel={handleCancel} projectId={projectId} />}
    />
  );
});

export default SelectMultiServiceTag;
