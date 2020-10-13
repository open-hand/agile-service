import React, { useCallback, useState, useMemo } from 'react';
import {
  CheckBox, Button, TextField, Icon,
} from 'choerodon-ui/pro';
import './FieldList.less';
import { SelectFieldProps } from '.';

const prefix = 'c7nagile-choose-field-list';

type Props = {
  closeMenu: () => void,
  value: string[]
  onSelect: (code: string | string[]) => void
  onUnSelect: (code: string | string[]) => void
} & Pick<SelectFieldProps, 'systemFields' | 'customFields'>

const FieldList: React.FC<Props> = ({
  closeMenu, value, onSelect, onUnSelect, systemFields, customFields,
}) => {
  const [searchText, setSearchText] = useState('');
  const codes = useMemo(() => [...systemFields, ...customFields].map(({ code }) => code), [customFields, systemFields]);
  const hasSelect = useMemo(() => value.length > 0, [value.length]);
  const hasSelectAll = useMemo(() => value.length === codes.length, [codes.length, value.length]);
  const isChecked = useCallback((code: string) => value.includes(code), [value]);
  function handleChange(code: string | string[], select: boolean) {
    if (select) {
      onSelect(code);
    } else {
      onUnSelect(code);
    }
  }
  return (
    <div
      className={prefix}
    >
      <div className={`${prefix}-search`}>
        <TextField
          style={{ flex: 1 }}
          value={searchText}
          onChange={(v) => {
            setSearchText(v);
          }}
          prefix={<Icon type="search" />}
          placeholder="输入文字以进行过滤"
          clearButton
        />
      </div>
      <div className={`${prefix}-header`}>
        <CheckBox
          indeterminate={!hasSelectAll && hasSelect}
          checked={hasSelectAll}
          onChange={(checkAll) => {
            if (checkAll) {
              closeMenu(); // 避免焦点丢失时无法再次点击添加筛选
              // TODO:过滤disabled的code
              handleChange(codes, true);
            } else {
              handleChange(codes, false);
            }
          }}
        >
          全选
        </CheckBox>
        <Button
          style={{ display: hasSelect ? 'inline-block' : 'none' }}
          onClick={() => {
            handleChange(codes, false);
          }}
        >
          清除筛选项
        </Button>
      </div>
      <div className={`${prefix}-content`}>
        {systemFields.length > 0 && (
          <div className={`${prefix}-section`}>
            <div className={`${prefix}-title`}>预定义字段</div>
            <div className={`${prefix}-list`}>
              {systemFields.map((field) => {
                const { title, code, disabled } = field;
                const checked = isChecked(code);
                return (
                  <div className={`${prefix}-item`} key={code}>
                    <CheckBox
                      value={code}
                      disabled={disabled}
                      checked={checked}
                      onChange={() => handleChange(code, !checked)}
                    >
                      {title}
                    </CheckBox>
                  </div>
                );
              })}
            </div>
          </div>
        )}
        {customFields.length > 0 && (
          <div className={`${prefix}-section`}>
            <div className={`${prefix}-title`}>自定义字段</div>
            <div className={`${prefix}-list`}>
              {customFields.map((field) => {
                const { title, code, disabled } = field;
                const checked = isChecked(code);
                return (
                  <div className={`${prefix}-item`} key={code}>
                    <CheckBox
                      value={code}
                      disabled={disabled}
                      checked={checked}
                      onChange={() => handleChange(code, !checked)}
                    >
                      {title}
                    </CheckBox>
                  </div>
                );
              })}
            </div>
          </div>
        )}
      </div>
    </div>
  );
};
export default FieldList;
