import React, { useCallback, useState, useMemo } from 'react';
import {
  CheckBox, Button, TextField, Icon,
} from 'choerodon-ui/pro';
import { forEach, filter } from 'lodash';
import './FieldList.less';
import { useDebounceFn } from 'ahooks';

const prefix = 'c7nagile-choose-field-list';

interface Group {
  title: string
  options: {
    title: string
    code: string,
    disabled?: boolean
  }[]
}
type Props = {
  closeMenu: () => void,
  value: string[]
  onSelect: (code: string | string[]) => void
  onUnSelect: (code: string | string[]) => void
  groups: Group[]
}

const FieldList: React.FC<Props> = ({
  closeMenu, value, onSelect, onUnSelect, groups,
}) => {
  const [searchText, setSearchText] = useState('');
  const codes = useMemo(() => groups.reduce((res, current) => [...res, ...current.options.map((o) => o.code)], []), [groups]);
  const newGroups = useMemo(() => {
    if (searchText && searchText !== '') {
      const newArr: Group[] = [];
      forEach(groups, (group: Group) => {
        const newOptions = filter(group.options || [], (option) => option?.title?.indexOf(searchText) > -1);
        newArr.push({ ...group, options: newOptions });
      });
      return newArr;
    }
    return groups;
  }, [groups, searchText]);
  const hasSelect = useMemo(() => value.length > 0, [value.length]);
  const hasSelectAll = useMemo(() => value.length === codes.length, [codes.length, value.length]);
  const isChecked = useCallback((code: string) => value.includes(code), [value]);
  const { run: handleInput } = useDebounceFn((searchValue: string) => {
    setSearchText(searchValue);
  }, { wait: 540 });
  const handleChange = useCallback((code: string | string[], select: boolean) => {
    if (select) {
      onSelect(code);
    } else {
      onUnSelect(code);
    }
  }, [onSelect, onUnSelect]);
  const renderGroup = useCallback((group: Group) => group.options.length > 0 && (
    <div className={`${prefix}-section`} key={group.title}>
      <div className={`${prefix}-title`}>{group.title}</div>
      <div className={`${prefix}-list`}>
        {group.options.map(({ code, disabled, title }) => {
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
  ), [handleChange, isChecked]);
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
          onInput={(e: any) => handleInput(e.target.value)}
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
        {newGroups.map((group) => renderGroup(group))}
      </div>
    </div>
  );
};
export default FieldList;
