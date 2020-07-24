import React, {
  useState, useMemo, useEffect, useRef, useImperativeHandle,
} from 'react';
import { unstable_batchedUpdates as batchedUpdates } from 'react-dom';
import { Button, DataSet } from 'choerodon-ui/pro';
import { SearchMatcher } from 'choerodon-ui/pro/lib/select/Select';
import { Renderer } from 'choerodon-ui/pro/lib/field/FormField';
import { debounce } from 'lodash';
import FragmentForSearch from './FragmentForSearch';
import styles from './index.less';

type MiddleWare<T> = (data: T[]) => T[];
function applyMiddleWares<T>(data: T[], middleWares: MiddleWare<T>[]): T[] {
  return middleWares.reduce((preData, middleWare) => middleWare(preData), data);
}
function noop<T>(data: T) {
  return data;
}
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
  page?: number
}

export interface SelectConfig<T = {}> {
  name: string
  textField: string
  valueField: string
  optionRenderer?: (item: T) => JSX.Element
  renderer?: (item: T) => JSX.Element
  request: ({ filter, page }: LoadConfig) => Promise<T[] | { list: T[], hasNextPage: boolean }>
  middleWare?: MiddleWare<T>,
  paging?: boolean
  props?: object
}
interface RefHandle {
  refresh: (config?: LoadConfig) => void
}
export default function useSelect<T extends {}>(config: SelectConfig<T>, ref?: React.MutableRefObject<RefHandle>) {
  const [data, setData] = useState<T[]>([]);
  const [currentPage, setPage] = useState(1);
  const [canLoadMore, setCanLoadMore] = useState(false);
  const textRef = useRef<string>('');
  const defaultRender = (item: T) => getValueByPath(item, textField);
  const {
    textField = 'name',
    valueField = 'id',
    optionRenderer = defaultRender,
    renderer,
    request,
    middleWare = noop,
    paging = true,
    props,
  } = config;
  // 不分页时，本地搜索
  const localSearch = !paging;
  const loadData = async ({ filter = textRef.current, page = 1 }: LoadConfig = {} as LoadConfig) => {
    const res = await request({ filter, page });
    batchedUpdates(() => {
      if (paging) {
        const { list, hasNextPage } = res as { list: T[], hasNextPage: boolean };
        setData(page > 1 ? data.concat(list) : list);
        setPage(page);
        setCanLoadMore(hasNextPage);
      } else {
        setData(res as T[]);
      }
    });
  };
  const searchData = useMemo(() => debounce((filter: string) => {
    loadData({ filter });
  }, 500), []);
  useEffect(() => {
    loadData({ filter: '' });
  }, [config]);
  useImperativeHandle<Object, RefHandle>(ref, () => ({
    refresh: loadData,
  }));
  const handleLoadMore = () => {
    loadData({ page: currentPage + 1 });
  };
  const handleInput = (e: React.ChangeEvent<HTMLInputElement>) => {
    const { value } = e.target;
    textRef.current = value;
    if (!localSearch) {
      searchData(value);
    }
  };
  const filterOptions: SearchMatcher = ({
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
  };
  let finalData: Array<T | { loadMoreButton: boolean }> = applyMiddleWares<T>(data, [middleWare]);
  if (canLoadMore) {
    finalData = [...finalData, { loadMoreButton: true }];
  }
  const loadMoreButton = (
    <Button
      onClick={(e) => {
        e.stopPropagation();
        handleLoadMore();
      }}
      style={{ margin: '-4px -12px', width: 'calc(100% + 24px)' }}
    >
      加载更多
    </Button>
  );
  const options = new DataSet({ data: finalData, paging: false });
  const renderOption: Renderer = ({ record }) => {
    if (!record) {
      return null;
    }
    if (record.get('loadMoreButton') === true) {
      return loadMoreButton;
    } else {
      return optionRenderer(record.toData());
    }
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
    ...props,
  };
  return selectProps;
}
export { FragmentForSearch };
