import React, {
  useState, useEffect, useCallback, useMemo,
} from 'react';
import { observer } from 'mobx-react-lite';
import { includes } from 'lodash';
import classNames from 'classnames';
import { IIssueType } from '@/common/types';
import useIssueTypes from '@/hooks/data/useIssueTypes';
import styles from './index.less';
import Switch from '@/components/switch';

type ChangeSelected = (code: string) => void

interface Props {
  selectedType?: string,
  visibleIssueTypeCategory?: 'all' | 'custom' | 'initial' /** @default 'all'  'custom' 自定义工作项类型  'initial' 初始工作项类型 */
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
  const {
    data: issueTypes, isFetchedAfterMount,
  } = useIssueTypes(undefined);
  const handleSelectType = useCallback((id: string) => {
    if (setSelectedType) {
      setSelectedType(id);
    } else {
      setSelected(id);
    }
  }, [setSelectedType]);

  useEffect(() => {
    if (isFetchedAfterMount) {
      const newIssueTypes = (issueTypes || []).filter((item: IIssueType) => handleFilterIssueType(item));
      if (!selectedType || (selectedType && !newIssueTypes.find((item: IIssueType) => item.id === selectedType))) {
        handleSelectType((newIssueTypes && newIssueTypes[0] && newIssueTypes[0].id) as string);
      }
      if (selectedType && selected !== selectedType && newIssueTypes.find((item: IIssueType) => item.id === selectedType)) {
        setSelected(selectedType);
      }
    }
  }, [handleFilterIssueType, handleSelectType, isFetchedAfterMount, issueTypes, selected, selectedType]);
  console.log('issueTypes', issueTypes);
  return (
    <Switch
      value={selected}
      className={styles.issueTypeTab}
      options={(issueTypes || []).filter((item: IIssueType) => handleFilterIssueType(item))
        .map((item:IIssueType) => ({ text: item.name, value: item.id }))}
      wrap
      onChange={(v) => handleSelectType(v)}
    />
  );
};

export default observer(IssueTypeTab);
