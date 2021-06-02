import React, {
  useMemo, forwardRef, useEffect, useRef, useCallback, useState,
} from 'react';
import {
  Select,
} from 'choerodon-ui/pro';
import { toJS } from 'mobx';
import { debounce, groupBy, pick } from 'lodash';
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
            console.log('binRef...focus');
            innerRef.current?.trigger?.delaySetPopupHidden(false, 150);
          },
        }),
      });
      ref && Object.assign(ref, { current: innerRef.current });
    }
  }, [ref]);
  const handlePopupHidden = debounce((hidden) => {
    hidden ? setEditValue(undefined) : setEditValue(handleProcessValue());
    hidden && onBlur && onBlur();
    onPopupHidden && onPopupHidden(hidden);
  }, 230);
  function handleCancel() {
    setEditValue(undefined);
    innerRef.current?.trigger?.delaySetPopupHidden(true, 0); // 关闭下拉框
    // handlePopupHidden(true);
  }
  useEffect(() => {
    console.log('tag. come');
    return () => {
      console.log('tag.. leave');
      onPopupHidden && onPopupHidden(true);
    };
  }, []);
  function handleSave(data: IMultiServiceTagItemProps[]) {
    onChange && onChange(data);
    setEditValue(undefined); // 置空编辑值 隐藏下拉框
    innerRef.current?.trigger?.delaySetPopupHidden(true, 0); // 关闭下拉框

    // handleCancel();
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

  const wrapProps = useMemo(() => pick(otherProps, ['style', 'className']), [otherProps]);
  return (
    <div id={componentId} {...wrapProps}>
      <Component
        ref={handleBindRef}
        value={value}
        multiple
        primitiveValue={false}
        // triggerHiddenDelay={100}
        onPopupHiddenChange={(hidden) => {
          console.log('onPopupHiddenChange..', hidden);
          // onChange();
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
          console.log('onChange', v);
        }}
        onBlur={(e) => { console.log('e....onBlur', e); }}
        {...otherProps}
        dropdownMatchSelectWidth={false}
        popupContent={editValue ? <MultiServiceTag mode={mode} onOK={handleSave} data={editValue} onCancel={handleCancel} projectId={projectId} /> : <div />}
      />
    </div>
  );
});

export default SelectMultiServiceTag;
