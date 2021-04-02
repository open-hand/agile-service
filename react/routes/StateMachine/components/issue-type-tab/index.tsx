import React, {
  useState, useEffect, useCallback, useMemo,
} from 'react';
import { observer } from 'mobx-react-lite';
import { IIssueType } from '@/common/types';
import useIssueTypes from '@/hooks/data/useIssueTypes';
import { includes } from 'lodash';
import classNames from 'classnames';
import styles from './index.less';

type ChangeSelected = (code: string) => void

interface Props {
  selectedType?: string,
  visibleIssueTypeCategory?: 'all' | 'custom' | 'initial' /** @default 'all'  'custom' 自定义问题类型  'initial' 初始问题类型 */
  setSelectedType?: ChangeSelected,
  excludeTypes?: string[]
  brighter?: boolean
}

const IssueTypeTab: React.FC<Props> = ({
  selectedType, setSelectedType, excludeTypes = [], brighter, visibleIssueTypeCategory = 'all',
}) => {
  const [selected, setSelected] = useState(selectedType || '');
  const hiddenCustomType = useMemo(() => (typeof (visibleIssueTypeCategory) === 'string' && ['all', 'custom', 'initial'].includes(visibleIssueTypeCategory) ? visibleIssueTypeCategory !== 'all' : false), [visibleIssueTypeCategory]);
  const handleFilterIssueType = useCallback((item: IIssueType): boolean => {
    const result = !includes(excludeTypes, item.typeCode);
    if (!result) {
      return false;
    }
    return visibleIssueTypeCategory === 'custom' ? !item.initialize : (!hiddenCustomType || item.initialize);
  }, [excludeTypes, hiddenCustomType, visibleIssueTypeCategory]);
  const { data: issueTypes } = useIssueTypes();
  const handleSelectType = useCallback((id: string) => {
    if (setSelectedType) {
      setSelectedType(id);
    } else {
      setSelected(id);
    }
  }, [setSelectedType]);

  useEffect(() => {
    const newIssueTypes = (issueTypes || []).filter((item: IIssueType) => handleFilterIssueType(item));
    if (!selectedType || (selectedType && !newIssueTypes.find((item: IIssueType) => item.id === selectedType))) {
      handleSelectType((newIssueTypes && newIssueTypes[0] && newIssueTypes[0].id) as string);
    }
    if (selectedType && selected !== selectedType && newIssueTypes.find((item: IIssueType) => item.id === selectedType)) {
      setSelected(selectedType);
    }
  }, [handleSelectType, selectedType, setSelectedType, issueTypes, selected, excludeTypes, handleFilterIssueType]);

  return (
    <div className={styles.issueTypeTab}>
      {
        (issueTypes || []).filter((item: IIssueType) => handleFilterIssueType(item)).map((item: IIssueType) => (
          <span
            // className={`${styles.issueTypeTabItem} ${item.id === selected ? styles.selected : ''}`}
            className={classNames({
              [styles.issueTypeTabItem]: true,
              [styles.selected]: item.id === selected,
              [styles.brighter]: item.id === selected && brighter,
            })}
            role="none"
            onClick={() => handleSelectType(item.id)}
          >
            {item.name}
          </span>
        ))
      }
    </div>
  );
};

export default observer(IssueTypeTab);
