import React, {
  useMemo, forwardRef, useEffect, useRef, useCallback, useState,
} from 'react';
import {
  Select, DataSet, Form, Progress, Button,
} from 'choerodon-ui/pro';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { observer, useForceUpdate } from 'mobx-react-lite';
import { toJS } from 'mobx';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { devOpsApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { ILabel } from '@/common/types';
import FlatSelect from '@/components/flat-select';
import { getProjectId } from '@/utils/common';
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
const MultiServiceTag: React.FC<{ onOK: (records: Record[]) => void, projectId?: string, onCancel: () => void, originData: any[] }> = observer(({
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
        console.log('update..', value, name);
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
  function handleSave() {
    const data = ds.toJSONData();
    onOK(ds.records);
  }
  const handleCreate = useCallback(() => {
    ds.create();
  }, [ds]);
  function handleCancel() {
    onCancel();
  }
  useEffect(() => {
    function loadData() {
      devOpsApi.project(projectId).loadActiveService().then((res: any) => {
        const data = res.map((item: any) => ({ ...item, meaning: item.name, value: item.id }));
        serviceOptionDs.loadData(data);
        if (originData && originData.length > 0) {
          console.log(originData.map((item) => {
            const appService = data.find((i: any) => i.code === item.appServiceCode);
            return { ...item, appService };
          }));
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
  return (
    <div role="none" className={`${prefixCls}-content`} onClick={(e) => e.stopPropagation()} onMouseDown={(e) => e.stopPropagation()}>
      <Form dataSet={ds}>
        {ds.records.flatMap((record) => (
          [<SelectAppService name="appService" options={serviceOptionDs} record={record} primitiveValue onClick={(e) => e.stopPropagation()} />,
            <SelectGitTags key={`select-tag-${record.get('appServiceId')}`} applicationId={record.get('appServiceId')} name="tagName" multiple record={record} />]
        ))}
        {ds.length > 0 && <Button icon="add" color={'primary' as any} onClick={handleCreate}>添加筛选</Button>}
      </Form>
      <div className={`${prefixCls}-btns`}>
        <Button funcType={'raised' as any} color={'primary' as any} onClick={handleSave}>确定</Button>
        <Button funcType={'raised' as any} onClick={handleCancel}>取消</Button>

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
  function handleSave(data: Record[]) {
    innerRef.current?.choose(data);
    const changeData: any[] = [];
    data.map((i) => i.toJSONData()).forEach((item) => {
      changeData.push(...item.tagName.map((tag: string) => ({ tagName: tag, appServiceCode: item.appServiceCode, projectId: projectId || getProjectId() })));
    });
    console.log('handleSave...', changeData);
    onChange && onChange(changeData, []);
  }
  const value = useMemo(() => {
    console.log('value...', propsValue);
    return toJS(propsValue)?.map((item: any) => (item.appServiceCode ? `${item.appServiceCode}:${item.tagName}` : item.tagName));
  }, [propsValue]);

  return (
    <Component
      ref={handleBindRef}
      value={value}
      // onBlur={() => {
      //   console.log('blur..');
      //   innerRef.current?.collapse();
      // }}
      onPopupHiddenChange={(hidden) => {
        console.log('blur..', onBlur);
        // onChange();
        hidden && onBlur && onBlur({} as any);
        // setMode(dateType === 'datetime' ? 'dateTime' : dateType);
      }}
      // getPopupContainer={(node) => node.parentNode as any}
      trigger={['click'] as any}
      onChange={(v) => {
        console.log('onChange', v);
      }}
      {...otherProps}
      dropdownMatchSelectWidth={false}
      popupContent={<MultiServiceTag onOK={handleSave} originData={propsValue} onCancel={() => innerRef.current?.collapse()} projectId={projectId} />}
    />
  );
});

export default SelectMultiServiceTag;
