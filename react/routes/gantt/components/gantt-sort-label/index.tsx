import React, {
  useState, useCallback, useMemo, useContext, useEffect,
} from 'react';
import { Icon } from 'choerodon-ui/pro';
import classNames from 'classnames';
import { find, findIndex, noop } from 'lodash';
import { useMount, usePersistFn } from 'ahooks';
import styles from './index.less';
import Context from '../../context';
import { ganttApi, IGanttSortDataItem } from '@/api';

interface IGanttSortLabelOnChange {
  (dataKeyValues: IGanttSortLabelSortItem[]): void
  (dataKey: string, sorted: 'asc' | 'desc' | undefined): void
}
export interface IGanttSortLabelProps {
  dataKey: string
  /** 是否启用本字段联合其它字段排序 @default  false  */
  unionSort?: boolean
  onChange?: IGanttSortLabelOnChange
}
export interface IGanttSortLabelSortItem extends Pick<IGanttSortLabelProps, 'dataKey'> {
  sorted: 'asc' | 'desc' | undefined
  // fieldCode: string
}
interface IGanttSortLabelHookData {
  origin: Array<IGanttSortLabelSortItem>
  data: string[]
  loading: boolean
}
interface IUseGanttSortLabelConfigProps {
  projectId?: string
}
export function useGanttSortLabel({ projectId }: IUseGanttSortLabelConfigProps): [IGanttSortLabelHookData, IGanttSortLabelProps['onChange']] {
  const [sortedList, setSortedList] = useState<Array<IGanttSortLabelSortItem>>([]);
  const [loading, setLoading] = useState(true);
  const handleChange = useCallback((dataKey: string | IGanttSortLabelSortItem[], sorted?: 'asc' | 'desc' | undefined) => {
    setSortedList((oldList) => {
      let temp: Array<IGanttSortLabelSortItem | undefined> = [...oldList];

      if (typeof dataKey === 'string') {
        const deleteItemIndex = findIndex(temp, { dataKey });
        const addItem = sorted ? { dataKey, sorted } : undefined;
        temp.splice(deleteItemIndex, 1);
        temp.push(addItem);
      } else {
        temp = temp.filter((item) => !dataKey.some((i) => i.dataKey === item?.dataKey));
        temp.push(...dataKey.filter((item) => item.sorted));
      }

      // if (deleteItemIndex !== -1) {
      //   temp[deleteItemIndex] = addItem;
      // } else {
      //   temp.push(addItem);
      // }
      const newSortedList = temp.filter(Boolean) as IGanttSortLabelSortItem[];
      ganttApi.project(projectId).saveSort(newSortedList.filter((item) => item.sorted).map((item) => ({ property: item.dataKey, direction: item.sorted! })));
      return newSortedList;
    });
  }, [projectId]);

  useEffect(() => {
    projectId && ganttApi.project(projectId).loadSort().then((res: IGanttSortDataItem[]) => {
      setSortedList(res.map((item) => ({ dataKey: item.property, sorted: item.direction })) || []);
      setLoading(false);
    }).catch(() => {
      setLoading(false);
    });
  }, [projectId]);
  const data = useMemo(() => sortedList.map((item) => `${item.dataKey},${item.sorted}`), [sortedList]);
  return [{ origin: sortedList, data, loading }, handleChange];
}
const GanttSortLabel: React.FC<IGanttSortLabelProps> = ({
  children, unionSort, dataKey, onChange: propsOnChange,
}) => {
  const [sorted, setSorted] = useState<'desc' | 'asc' | undefined>();
  const { sortedList } = useContext(Context);
  const onChange = usePersistFn(propsOnChange || noop);
  useEffect(() => {
    if (!unionSort) {
      const sort = find(sortedList, { dataKey });
      setSorted(sort?.sorted);
    }
  }, [dataKey, sortedList, unionSort]);
  return (
    <div
      role="none"
      className={styles.wrap}
      onClick={() => {
        setSorted((old) => {
          let temp: any = 'asc';
          if (old) {
            temp = old === 'asc' ? 'desc' : undefined;
          }
          onChange(dataKey, temp);
          return temp;
        });
      }}
    >
      <span className={styles.ellipsis}>{children}</span>
      <Icon
        type="arrow_upward"
        className={classNames(styles.sort, { [styles.sort_active]: !!sorted, [styles.sort_desc]: sorted === 'desc' })}
      />
    </div>
  );
};
export default GanttSortLabel;
