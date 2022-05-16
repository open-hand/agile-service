import React, {
  useCallback, useMemo, useState, useRef, useEffect,
} from 'react';
import { find, uniq, pull } from 'lodash';
import { Button, Icon } from 'choerodon-ui/pro';
import classNames from 'classnames';
import { LabelLayout } from 'choerodon-ui/pro/lib/form/enum';
import { FuncType } from 'choerodon-ui/pro/lib/button/enum';
import { BasicTarget } from 'ahooks/lib/utils/dom';
import { usePersistFn, useSize } from 'ahooks';
import Field from '@/components/field';
import SelectField, { SelectFieldProps } from '@/components/field/select-field';
import { ISystemField, ICustomField, IFilterField } from '.';
import { getFlatElement, renderGroupedFields, renderFields } from './utils';
import styles from './Filter.less';
import useFormatMessage from '@/hooks/useFormatMessage';

export interface IFilter {
  [key: string]: any
}
export type IRenderFields = (arg: {
  fields: IFilterField[]
  getFieldElement: (field: IFilterField) => { element: React.ReactElement, removeButton: React.ReactElement | null }
  selectField: React.ReactElement | null
  resetButton: React.ReactElement | null
  foldButton?: React.ReactElement | null
  folded?: boolean | undefined,
  overflowLine?: boolean,
  filterRef?: React.MutableRefObject<HTMLDivElement | null>,

}) => React.ReactElement

export interface FilterProps {
  filter: IFilter
  onFilterChange: (filter: IFilter) => void
  selected: string[]
  onSelectChange: (selected: string[]) => void
  systemFields: ISystemField[]
  customFields: ICustomField[]
  alwaysRenderFields?: string[]
  render?: (field: IFilterField, element: React.ReactNode) => React.ReactNode
  renderer?: IRenderFields
  removeButton?: boolean | React.ReactNode
  grouped?: boolean
  flat?: boolean
  selectFieldGroups?: SelectFieldProps['groups']
  resetAll?: boolean
}
const Filter: React.FC<FilterProps> = ({
  systemFields,
  customFields,
  alwaysRenderFields = [],
  render,
  renderer,
  onSelectChange,
  onFilterChange,
  selected,
  removeButton,
  grouped,
  flat,
  filter,
  selectFieldGroups,
  resetAll,
}) => {
  const formatMessage = useFormatMessage();
  const [folded, setFolded] = useState<boolean | undefined>();
  const [overflowLine, setOverflowLine] = useState<boolean>(false);
  const filterRef = useRef<HTMLDivElement | null>(null);
  const getNewFilter = usePersistFn((preFilter: IFilter, code: string, v: any) => {
    // 保证默认显示的字段在值清空时不会出现保存筛选
    const value = alwaysRenderFields.includes(code) ? v ?? undefined : v ?? null;
    const clonedFilter = { ...preFilter };
    if (value === undefined) {
      delete clonedFilter[code];
    } else {
      clonedFilter[code] = value;
    }
    return clonedFilter;
  });
  const handleFilterChange = usePersistFn((code: string, v: any) => {
    onFilterChange(getNewFilter(filter, code, v));
  });
  const handleSelect = useCallback((select: string[]) => {
    const newValue = uniq([...selected, ...select]);
    onSelectChange(newValue);
    onFilterChange(select.reduce((res, current) => getNewFilter(res, current, null), filter));
  }, [filter, getNewFilter, onFilterChange, onSelectChange, selected]);
  const handleUnSelect = useCallback((unselect: string[]) => {
    const newValue = [...selected];
    const clonedFilter = { ...filter };
    let changedFilter = false;
    unselect.forEach((code) => {
      pull(newValue, code);
      if (Object.prototype.hasOwnProperty.call(filter, code)) {
        changedFilter = true;
        delete clonedFilter[code];
      }
    });
    onSelectChange(newValue);
    if (changedFilter) {
      onFilterChange(clonedFilter);
    }
  }, [filter, onSelectChange, onFilterChange, selected]);
  const handleSelectChange = useCallback((codes: string[], select: boolean) => {
    if (select) {
      handleSelect(codes);
    } else {
      handleUnSelect(codes);
    }
  }, [handleSelect, handleUnSelect]);
  const clearAllSelected = useCallback(() => {
    handleSelectChange(selected, false);
  }, [handleSelectChange, selected]);
  const totalFields = useMemo(() => [...systemFields, ...customFields], [customFields, systemFields]);
  const selectedFields = useMemo(() => alwaysRenderFields.concat(selected).reduce((result: IFilterField[], code) => {
    const field = find(totalFields, { code });
    if (field) {
      result.push(field as IFilterField);
    }
    return result;
  }, []), [alwaysRenderFields, selected, totalFields]);
  const systemFieldsWithoutAlwaysRender = useMemo(() => systemFields.filter(({ code }) => !alwaysRenderFields.includes(code)), [alwaysRenderFields, systemFields]);
  const groups = useMemo(() => selectFieldGroups || [
    ...(systemFieldsWithoutAlwaysRender.length > 0 ? [{
      title: '系统字段',
      options: systemFieldsWithoutAlwaysRender.map((f) => ({
        // @ts-ignore
        title: f.nameKey ? formatMessage({ id: f.nameKey }) : f.title,
        code: f.code,
      })),
    }] : []),
    {
      title: '自定义字段',
      options: customFields.map((f: any) => ({
        title: f.title,
        code: f.code,
      })),
    },
  ], [customFields, formatMessage, selectFieldGroups, systemFieldsWithoutAlwaysRender]);
  const renderRemoveButton = useCallback((field: IFilterField) => {
    if (!removeButton || alwaysRenderFields.includes(field.code)) {
      return null;
    }
    if (removeButton === true) {
      return (
        <Button
          funcType={'flat' as any}
          icon="delete_sweep-o"
          style={{ marginLeft: 10, flexShrink: 0 }}
          onClick={() => {
            handleSelectChange([field.code], false);
          }}
        />
      );
    }
    if (React.isValidElement(removeButton)) {
      return React.cloneElement(removeButton, {
        onClick: () => {
          handleSelectChange([field.code], false);
        },
      });
    }
    return null;
  }, [alwaysRenderFields, handleSelectChange, removeButton]);
  const getEmptyValue = useCallback((v: any, {
    isSelect,
    isText,
    isTime,
  }) => {
    if (v) {
      return v;
    }
    if (isSelect) {
      return [];
    }
    if (isText) {
      return '';
    }
    if (isTime) {
      return { start: undefined, end: undefined };
    }
    return v;
  }, []);
  const renderFlatElement = useCallback((field: IFilterField, element: React.ReactNode) => {
    const flatElement = getFlatElement(field, element);
    return render ? render(field, flatElement) : flatElement;
  }, [render]);
  const renderField = useCallback((field: IFilterField) => {
    const isSelect = ['single', 'multiple', 'radio', 'checkbox', 'member'].includes(field.fieldType);
    const isText = ['input', 'text'].includes(field.fieldType);
    const isUser = ['member', 'multiMember'].includes(field.fieldType);
    const isTime = ['time', 'datetime', 'date'].includes(field.fieldType);
    const className = flat ? classNames({
      'c7n-pro-select-flat': isSelect,
      'c7n-pro-cascader-flat': isSelect,
    }) : undefined;
    // @ts-ignore
    // eslint-disable-next-line no-nested-ternary
    const placeholder = isTime ? [formatMessage({ id: 'agile.search.startTime' }), formatMessage({ id: 'agile.search.endTime' })] : (field.nameKey ? formatMessage({ id: field.nameKey }) : field.title);
    return {
      element: <Field
        style={{
          marginRight: 10, marginTop: flat ? 0 : 10, flex: 1, flexShrink: 1,
        }}
        render={flat ? renderFlatElement : render}
        mode="filter"
        field={field}
        // @ts-ignore
        label={field.nameKey ? formatMessage({ id: field.nameKey }) : field.title}
        // @ts-ignore
        flat={flat}
        // @ts-ignore
        placeholder={placeholder}
        required={field.required}
        value={getEmptyValue(filter[field.code], {
          isSelect,
          isText,
          isTime,
        })}
        onChange={(v: any) => {
          if (isTime && v && v.length > 0) {
            if (v[0] === undefined && v[1] === undefined) {
              handleFilterChange(field.code, undefined);
              return;
            }
            if ((v[0] && !v[1]) || (!v[0] && v[1])) {
              return;
            }
          }
          if (filter[field.code] === undefined && v === null) {
            return;
          }
          handleFilterChange(field.code, v);
        }}
        className={className}
        {...flat ? {
          dropdownMatchSelectWidth: false,
          maxTagCount: 3,
          maxTagTextLength: 5,
        } : {}}
        {...isUser ? {
          selected: filter[field.code],
        } : {}}
      />,
      removeButton: renderRemoveButton(field),
    };
  }, [flat, renderFlatElement, render, formatMessage, getEmptyValue, filter, renderRemoveButton, handleFilterChange]);
  const renderSelectField = useCallback(() => {
    if (groups.length === 0) {
      return null;
    }
    return (
      <SelectField
        groups={groups}
        value={selected}
        onChange={handleSelectChange}
        triggerElement={flat ? (
          <Button>
            <span style={{
              display: 'flex', alignItems: 'center', fontWeight: 500,
            }}
            >
              {formatMessage({ id: 'agile.common.add.filter' })}
              <Icon type="arrow_drop_down" />
            </span>
          </Button>
        ) : undefined}
      />
    );
  }, [flat, formatMessage, groups, handleSelectChange, selected]);
  const resetFilter = useCallback(() => {
    onFilterChange({});
    if (resetAll) {
      onSelectChange([]);
    } else {
      clearAllSelected();
    }
  }, [clearAllSelected, onFilterChange, onSelectChange, resetAll]);

  const expandFilter = useCallback(() => {
    setFolded(!folded);
  }, [folded]);

  const renderResetButton = useCallback(() => {
    if (!Object.keys(filter).some((key) => filter[key] !== undefined && filter[key] !== '')) {
      return null;
    }
    return (
      <Button className={styles.btn} funcType={'flat' as FuncType} onClick={resetFilter}>
        {formatMessage({ id: 'agile.common.reset' })}
      </Button>
    );
  }, [filter, formatMessage, resetFilter]);

  const renderFoldButton = useCallback(() => (
    <>
      {
        (overflowLine || folded === true) && (
          <Button className={`${styles.btn} ${folded === true ? styles.foldedBtn : ''}`} onClick={expandFilter}>
            <Icon type={folded ? 'expand_more' : 'expand_less'} />
          </Button>
        )
      }
    </>
  ), [expandFilter, folded, overflowLine]);

  const selectField = renderSelectField();
  const resetButton = renderResetButton();
  const foldButton = renderFoldButton();

  const searchSize = useSize(filterRef as BasicTarget);

  useEffect(() => {
    setOverflowLine((searchSize.height || 0) > 50);
  }, [searchSize]);

  if (renderer) {
    return renderer({
      fields: selectedFields, getFieldElement: renderField, selectField, resetButton, foldButton, folded, overflowLine, filterRef,
    });
  }
  return grouped
    ? renderGroupedFields({
      fields: selectedFields, getFieldElement: renderField, selectField, resetButton, foldButton, folded, overflowLine, filterRef,
    })
    : renderFields({
      fields: selectedFields, getFieldElement: renderField, selectField, resetButton,
    });
};
export default Filter;
