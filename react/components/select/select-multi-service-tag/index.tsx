import React, {
  useMemo, forwardRef, useEffect, useRef, useCallback, useState,
} from 'react';
import {
  Select,
} from 'choerodon-ui/pro';
import { toJS } from 'mobx';
import { groupBy } from 'lodash';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { ILabel } from '@/common/types';
import { FlatSelect } from '@choerodon/components';
import { randomString } from '@/utils/random';

import './index.less';
import useClickOut from '@/hooks/useClickOut';
import MultiServiceTag, { IMultiServiceTagItemProps, IMultiServiceTagProps } from './MultiServiceTag';

interface Props extends Partial<SelectProps> {
  dataRef?: React.RefObject<Array<any>>
  valueField?: string
  afterLoad?: (sprints: ILabel[]) => void
  onChange?: (data: IMultiServiceTagItemProps[]) => void
  onBlur?: () => void
  onPopupHidden?: (hidden: boolean) => void
  applicationId?: string | null
  flat?: boolean
  mode?: 'program'
  projectId?: string
}

const SelectMultiServiceTag: React.FC<Props> = forwardRef(({
  dataRef, valueField, afterLoad, flat, applicationId, projectId, mode, onBlur, onChange, onPopupHidden, value: propsValue, ...otherProps
}, ref: React.Ref<Select>) => {
  const innerRef = useRef<Select | null>();
  const handleBindRef = useCallback((newRef) => {
    if (newRef) {
      Object.assign(innerRef, {
        current: Object.assign(newRef, {
          focus: () => {
            innerRef.current?.trigger?.delaySetPopupHidden(false, 110);
          },
        }),
      });
      ref && Object.assign(ref, { current: innerRef.current });
    }
  }, [ref]);

  function handleCancel() {
    setEditValue(undefined);
    innerRef.current?.collapse();
    onPopupHidden && onPopupHidden(true);
  }
  function handleSave(data: IMultiServiceTagItemProps[]) {
    onChange && onChange(data);
    handleCancel();
  }
  const value = useMemo(() => toJS(propsValue)?.map((item: any) => ({ meaning: (item.appServiceCode ? `${item.appServiceCode}:${item.tagName}` : item.tagName), value: item })), [propsValue]);
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

  useEffect(() => {
    console.log('val.... useEffect mode', mode);
    // setEditValue(handleProcessValue());
  }, [mode]);
  return (
    <div id={componentId}>
      <Component
        ref={handleBindRef}
        value={value}
        multiple
        primitiveValue={false}
        onPopupHiddenChange={(hidden) => {
          console.log('blur..', hidden);
          // onChange();
          hidden ? setEditValue(undefined) : setEditValue(handleProcessValue());
          hidden && setTimeout(() => onPopupHidden && onPopupHidden(true), 100);
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
        popupContent={editValue ? <MultiServiceTag mode={mode} onOK={handleSave} data={editValue} onCancel={handleCancel} projectId={projectId} /> : <div />}
      />
    </div>
  );
});

export default SelectMultiServiceTag;
