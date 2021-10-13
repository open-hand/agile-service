import React, {
  useState, useCallback, useMemo,
} from 'react';
import { Icon } from 'choerodon-ui/pro';
import classNames from 'classnames';
import { findIndex, noop } from 'lodash';
import { usePersistFn } from 'ahooks';
import styles from './index.less';

export interface IGanttSortLabelProps {
  dataKey: string
  onChange?: (dataKey: string, sorted: 'asc' | 'desc' | undefined) => void
}
interface IGanttSortLabelSortItem extends Pick<IGanttSortLabelProps, 'dataKey'> {
  sorted: 'asc' | 'desc'
}
interface IGanttSortLabelHookData {
  origin: Array<IGanttSortLabelSortItem>
  data: string[]
}
export function useGanttSortLabel(): [IGanttSortLabelHookData, IGanttSortLabelProps['onChange']] {
  const [sortedList, setSortedList] = useState<Array<IGanttSortLabelSortItem>>([]);
  const handleChange = useCallback((dataKey: string, sorted: 'asc' | 'desc' | undefined) => {
    setSortedList((oldList) => {
      const deleteItemIndex = findIndex(oldList, { dataKey });
      const temp: Array<IGanttSortLabelSortItem | undefined> = [...oldList];
      const addItem = sorted ? { dataKey, sorted } : undefined;
      if (deleteItemIndex !== -1) {
        temp[deleteItemIndex] = addItem;
      } else {
        temp.push(addItem);
      }

      return temp.filter(Boolean) as IGanttSortLabelSortItem[];
    });
  }, []);

  const data = useMemo(() => sortedList.map((item) => `${item.dataKey},${item.sorted}`), [sortedList]);
  return [{ origin: sortedList, data }, handleChange];
}
const GanttSortLabel: React.FC<IGanttSortLabelProps> = ({ children, dataKey, onChange: propsOnChange }) => {
  const [sorted, setSorted] = useState<'desc' | 'asc' | undefined>();
  const onChange = usePersistFn(propsOnChange || noop);
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
