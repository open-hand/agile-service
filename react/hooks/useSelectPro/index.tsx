import React, {
  useMemo, useEffect, useRef, useImperativeHandle, useCallback,
} from 'react';
import { omit, debounce } from 'lodash';
import { Button, DataSet } from 'choerodon-ui/pro';
import { SearchMatcher } from 'choerodon-ui/pro/lib/select/Select';
import { Renderer } from 'choerodon-ui/pro/lib/field/FormField';

import FragmentForSearch from './FragmentForSearch';
import styles from './index.less';

/**
 * 从对象中获取值，可以传一个key或路径，比如 date.str
 * @param object
 * @param path
 */
function getValueByPath(object: object, path: string) {
  const paths: string[] = path.split('.');
  let result = object;
  while (paths.length > 0) {
    const key = paths.shift();
    if (Object.prototype.hasOwnProperty.call(object, key as string)) {
      // @ts-ignore
      result = result[key as string];
    } else {
      return undefined;
    }
  }
  return result;
}
export interface LoadConfig {
  filter?: string,
  nextPage?: number
}

export interface SelectConfig<T = {}> {
  data: T[]
  hasNextPage?: boolean
  page?: number
  name: string
  textField: string
  valueField: string
  optionRenderer?: (item: T) => JSX.Element
  renderer?: (item: T) => JSX.Element
  paging?: boolean
  props?: object
}

export interface RefHandle {
  refresh: (config?: LoadConfig) => void
}
export type UseSelectRef = React.MutableRefObject<RefHandle>
export default function useSelect<T extends { [key: string]: any }>(config: SelectConfig<T>, ref?: React.MutableRefObject<RefHandle>) {
  const textRef = useRef<string>('');
  const dataSetRef = useRef<DataSet>();
  const cacheRef = useRef<Map<any, T>>(new Map());
  const defaultRender = useCallback((item: T) => getValueByPath(item, config.textField), [config.textField]);
  const {
    data,
    hasNextPage,
    page = 1,
    textField = 'meaning',
    valueField = 'value',
    optionRenderer = defaultRender,
    paging = true,
    props,
  } = config;
  const renderer = useCallback(({ value, maxTagTextLength }) => {
    const item = cacheRef.current?.get(value);
    if (item) {
      const result = optionRenderer(item);
      return maxTagTextLength
        && typeof result === 'string'
        && (result as string).length > maxTagTextLength
        ? `${(result as string).slice(0, maxTagTextLength)}...`
        : result;
    }
    return null;
  }, [optionRenderer]);
  // 不分页时，本地搜索
  const localSearch = !paging;
  const loadData = useCallback(async ({ filter = textRef.current, nextPage = 1 }: LoadConfig = {} as LoadConfig) => {

  }, []);
  const searchData = useMemo(() => debounce((filter: string) => {
    loadData({ filter });
  }, 500), [loadData]);
  useEffect(() => {
    loadData({ filter: '' });
  }, [loadData]);
  useImperativeHandle<Object, RefHandle>(ref, () => ({
    refresh: loadData,
  }));
  const handleLoadMore = useCallback(() => {
    loadData({ nextPage: page + 1 });
  }, [page, loadData]);
  const handleInput = useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
    const { value } = e.target;
    textRef.current = value;
    if (!localSearch) {
      searchData(value);
    }
  }, [localSearch, searchData]);
  const filterOptions: SearchMatcher = useCallback(({
    record, text,
  }) => {
    // @ts-ignore
    const meaning = optionRenderer === defaultRender ? getValueByPath(record.data, textField) : optionRenderer(record.data);
    if (!meaning) {
      return true;
    }
    let name = '';
    // 一般情况，option的children是一个字符串
    if (typeof meaning === 'string') {
      name = meaning;
    } else if (React.isValidElement(meaning)) {
      // 其他情况, children是一个元素,那么约定这个元素上的name属性进行搜索
      // @ts-ignore
      // eslint-disable-next-line prefer-destructuring
      name = meaning.props.name;
    } else {
      return true;
    }
    return name.toLowerCase().indexOf(text.toLowerCase()) >= 0;
  }, [defaultRender, optionRenderer, textField]);
  const optionData: Array<T> = useMemo(() => data.map((item) => ({
    ...item,
    meaning: item[textField],
    value: item[valueField],
  })), [data, textField, valueField]);
  const finalData: Array<T | { loadMoreButton: boolean }> = useMemo(() => (hasNextPage ? [...optionData, { loadMoreButton: true }] : optionData), [hasNextPage, optionData]);
  const loadMoreButton = useMemo(() => (
    <Button
      onClick={(e) => {
        e.stopPropagation();
        handleLoadMore();
      }}
      style={{ margin: '-4px -12px', width: 'calc(100% + 24px)' }}
    >
      加载更多
    </Button>
  ), [handleLoadMore]);
  const options = useMemo(() => {
    if (!dataSetRef.current) {
      dataSetRef.current = new DataSet({
        data: finalData,
        paging: false,
      });
    } else {
      dataSetRef.current.loadData(finalData);
    }
    optionData.forEach((item) => {
      cacheRef.current?.set(item[valueField], item);
    });
    return dataSetRef.current;
  }, [finalData, optionData, valueField]);
  const renderOption: Renderer = ({ record }) => {
    if (!record) {
      return null;
    }
    if (record.get('loadMoreButton') === true) {
      return loadMoreButton;
    }
    return optionRenderer(record.toData());
  };
  const selectProps = {
    searchable: true,
    onInput: handleInput,
    onClear: () => {
      textRef.current = '';
      searchData('');
    },
    // 弹出时自动请求
    onPopupHiddenChange: (hidden: boolean) => {
      if (hidden === false && textRef.current !== '' && paging) {
        textRef.current = '';
        searchData('');
      }
    },
    searchMatcher: paging ? () => true : filterOptions,
    valueField,
    // 这里不传递textField，因为由useSelect来渲染
    textField,
    options,
    // @ts-ignore
    optionRenderer: renderOption,
    // TODO: 考虑如何获取record，来渲染，例如用户
    renderer,
    // renderer: renderer ? ({
    //   // @ts-ignore
    //   value, text, name, record, dataSet,
    // }) => {

    //   return (record ? renderer() : null);
    // } : undefined,
    // @ts-ignore
    onOption: ({ record }) => {
      if (record.get('loadMoreButton') === true) {
        return {
          className: styles.load_more,
          disabled: true,
        };
      }
      return {};
    },
    ...omit(props, 'renderer', 'optionRenderer'),
  };
  return [selectProps];
}
export { FragmentForSearch };
