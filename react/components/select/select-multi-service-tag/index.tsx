import React, {
  useMemo, forwardRef, useEffect, useRef, useCallback, useState,
} from 'react';
import {
  Select, DataSet,
} from 'choerodon-ui/pro';
import { toJS } from 'mobx';
import {
  debounce, groupBy, isEmpty, pick, set,
} from 'lodash';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import { ILabel } from '@/common/types';
import { randomString } from '@/utils/random';
import MultiServiceTag, { IMultiServiceTagItemProps, IMultiServiceTagProps } from './MultiServiceTag';
import './index.less';

export interface SelectMultiServiceTagProps extends Partial<SelectProps> {
  dataRef?: React.RefObject<Array<any>>
  valueField?: string
  afterLoad?: (sprints: ILabel[]) => void
  onChange?: (data: IMultiServiceTagItemProps[]) => void
  onBlur?: () => void
  onPopupHidden?: (hidden: boolean) => void
  applicationId?: string | null
  flat?: boolean
  /** 模式 @default 'project' */
  mode?: 'program' | 'project'
  projectId?: string
}

const SelectMultiServiceTag: React.FC<SelectMultiServiceTagProps> = forwardRef(({
  dataRef, valueField, afterLoad, flat, applicationId, projectId, mode, onBlur, onChange, onPopupHidden, value: propsValue, defaultValue, ...otherProps
}, ref: React.Ref<Select>) => {
  const innerRef = useRef<Select | null>();
  const handleBindRef = useCallback((newRef) => {
    if (newRef) {
      Object.assign(innerRef, {
        current: Object.assign(newRef, {
          focus: () => {
            // 重新实现focus，将time置为非空  避免click事件随后触发
            set((innerRef.current?.trigger || {}), 'focusTime', 1);
            set((innerRef.current?.trigger || {}), 'preClickTime', 1);
            innerRef.current?.trigger?.popupTask?.cancel();
            innerRef.current?.trigger?.delaySetPopupHidden(false, 150);
          },
        }),
      });
      ref && Object.assign(ref, { current: innerRef.current });
    }
  }, [ref]);
  const rendererSelectTag = (data: IMultiServiceTagItemProps) => (data && data.appServiceCode ? `${data.appServiceCode}:${data.tagName}` : data.tagName);
  const handleProcessPropsValue = (data?: any[]) => {
    if (isEmpty(toJS(data)) || !Array.isArray(toJS(data))) {
      return undefined;
    }
    return data!.filter(Boolean).map((item: any) => ({
      meaning: rendererSelectTag(item.value || item),
      value: item,
      uniqValue: `${item.projectId} ${item.appServiceCode} ${item.tagName}`,
    }));
  };
  const handlePopupHidden = debounce((hidden) => {
    hidden ? setEditValue(undefined) : setEditValue(handleProcessValue());
    hidden && onBlur && onBlur();
    onPopupHidden && onPopupHidden(hidden);
  }, 230);
  const handleCancel = useCallback(() => {
    setEditValue(undefined);
    innerRef.current?.trigger?.delaySetPopupHidden(true, 0); // 关闭下拉框
    // handlePopupHidden(true);
  }, []);
  useEffect(() => () => {
    onPopupHidden && onPopupHidden(true);
  }, []);
  const [value, setValue] = useState(() => handleProcessPropsValue(propsValue) || handleProcessPropsValue(defaultValue));
  const handleSave = useCallback((data: IMultiServiceTagItemProps[]) => {
    onChange && onChange(data);
    setValue(handleProcessPropsValue(data));
    otherProps.name && innerRef.current?.record?.set(otherProps.name, data);
    setEditValue(undefined); // 置空编辑值 隐藏下拉框
    innerRef.current?.trigger?.delaySetPopupHidden(true, 0); // 关闭下拉框
  }, []);
  const optionsDataSet = useMemo(() => new DataSet({ autoCreate: false, autoQuery: false, paging: false }), []);
  useEffect(() => {
    // 重新加载optionsDataSet数据  以显示值
    // innerRef.current?.choose()
    optionsDataSet.loadData(value?.map((i) => ({ meaning: i.meaning, value: i?.uniqValue })));
    // innerRef.current?.choose(optionsDataSet.loadData(value).map((i) => i));
  }, [optionsDataSet, otherProps.name, value]);
  useEffect(() => {
    setValue(handleProcessPropsValue(innerRef.current?.record ? innerRef.current.getValue() : propsValue));
  }, [propsValue]);

  const handleProcessValue = useCallback(() => {
    // 合并相同code的tag选项
    if (value && value.length > 0) {
      const groupObj = groupBy(value, (v) => `${v.value.appServiceCode}%${v.value.projectId}`);
      return Object.entries(groupObj).map(([item, tags]) => {
        const [code, codeProjectId] = item.split('%');
        return ({ appServiceCode: code, tagName: tags?.map((v) => v.value.tagName), projectId: codeProjectId });
      });
    }
    return [];
  }, [value]);

  const [editValue, setEditValue] = useState<IMultiServiceTagProps['data'] | undefined>(undefined);
  const Component = flat ? FlatSelect : Select;
  const componentId = useMemo(() => `select-multi-service-tag-${randomString(5)}`, []);

  const wrapProps = useMemo(() => pick(otherProps, ['style', 'className']), [otherProps]);
  return (
    // <div id={componentId} {...wrapProps}>
    <Component
      ref={handleBindRef}
      value={value?.map((i) => i.uniqValue)}
      multiple
      primitiveValue={false}
      renderer={({ text, value: renderValue }) => text || rendererSelectTag(renderValue)}
      options={optionsDataSet}
      onPopupHiddenChange={(hidden) => {
        handlePopupHidden(hidden);
      }}
      // getPopupContainer={(node) => document.getElementById(componentId) as HTMLElement}
      // getPopupContainer={() => document.body}
      trigger={['click'] as any}
      onChange={(v) => {
        let newValue = v;
        if (v && v.length > 0) {
          newValue = v.map((i: any) => i.value);
        }
        onChange && onChange(newValue);
      }}
      {...otherProps}
      dropdownMatchSelectWidth={false}
      popupContent={editValue ? <MultiServiceTag mode={mode} onOK={handleSave} data={editValue} onCancel={handleCancel} projectId={projectId} /> : <div />}
    />
    // </div>
  );
});

export default SelectMultiServiceTag;
