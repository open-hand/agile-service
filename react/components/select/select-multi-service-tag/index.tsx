import React, {
  useMemo, forwardRef, useEffect, useRef, useCallback,
} from 'react';
import {
  Select, DataSet, Form, Progress, Button,
} from 'choerodon-ui/pro';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { observer, useForceUpdate } from 'mobx-react-lite';
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
const MultiServiceTag: React.FC<{ onOK: (records: Record[]) => void }> = observer(({ onOK }) => {
  const ds = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      {
        name: 'appService', label: '应用服务', type: 'object' as any, ignore: 'always' as any,
      },
      { name: 'appServiceId', label: '应用服务', bind: 'appService.id' },
      { name: 'appServiceCode', label: '应用服务', bind: 'appService.code' },

      { name: 'tagName', label: 'Tag', parentField: 'appService' },
    ],
  }), []);
  function handleSave() {
    const data = ds.toJSONData();
    console.log('save..', data);
    onOK(ds.records);
  }
  return (
    <div role="none" className={`${prefixCls}-content`} onClick={(e) => e.stopPropagation()} onMouseDown={(e) => e.stopPropagation()}>
      <Form dataSet={ds} columns={2}>
        {ds.records.flatMap((record) => (
          [<SelectAppService name="appService" record={record} primitiveValue getPopupContainer={(node) => document.getElementsByClassName(`${prefixCls}-content`)[0] as any} />,
            <SelectGitTags key={`select-tag-${record.get('appServiceId')}`} applicationId={record.get('appServiceId')} name="tagName" multiple record={record} getPopupContainer={(node) => document.getElementsByClassName(`${prefixCls}-content`)[0] as any} />]
        ))}
      </Form>
      <Button funcType={'raised' as any} color={'primary' as any} onClick={handleSave}>确定</Button>
    </div>
  );
});
const SelectMultiServiceTag: React.FC<Props> = forwardRef(({
  dataRef, valueField, afterLoad, flat, applicationId, projectId, onBlur, onChange, ...otherProps
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
    const changeData:any[] = [];
    data.map((i) => i.toJSONData()).forEach((item) => {
      changeData.push(...item.tagName.map((tag: string) => ({ tagName: tag, appServiceCode: item.appServiceCode, projectId: projectId || getProjectId() })));
    });
    console.log('handleSave...', changeData);
    onChange && onChange(changeData, []);
  }
  return (
    <Component
      ref={handleBindRef}
      getPopupContainer={(node) => node.parentNode as any}
      trigger={['click'] as any}
      onChange={(v) => {
        console.log('onChange', v);
      }}
      {...otherProps}
      dropdownMatchSelectWidth={false}
      popupContent={<MultiServiceTag onOK={handleSave} />}
    />
  );
});

export default SelectMultiServiceTag;
