import React, { useState, useEffect, useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import { IIssueType } from '@/common/types';
import useIssueTypes from '@/hooks/data/useIssueTypes';
import { includes } from 'lodash';
import styles from './index.less';

type ChangeSelected = (code: string)=>void

interface Props {
  selectedType?: string,
  setSelectedType?: ChangeSelected,
  excludeTypes?: string[]
}

const IssueTypeTab: React.FC<Props> = ({ selectedType, setSelectedType, excludeTypes = [] }) => {
  const [selected, setSelected] = useState(selectedType || '');
  const { data: issueTypes } = useIssueTypes();
  const handleSelectType = useCallback((id: string) => {
    if (setSelectedType) {
      setSelectedType(id);
    } else {
      setSelected(id);
    }
  }, [setSelectedType]);

  useEffect(() => {
    if (!selectedType) {
      handleSelectType((issueTypes && issueTypes[0] && issueTypes[0].id) as string);
    }
    if (selectedType && selected !== selectedType) {
      setSelected(selectedType);
    }
  }, [handleSelectType, selectedType, setSelectedType, issueTypes, selected]);

  return (
    <div className={styles.issueTypeTab}>
      {
        (issueTypes || []).filter((item: IIssueType) => !includes(excludeTypes, item.typeCode)).map((item: IIssueType) => (
          <span
            className={`${styles.issueTypeTabItem} ${item.id === selected ? styles.selected : ''}`}
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
