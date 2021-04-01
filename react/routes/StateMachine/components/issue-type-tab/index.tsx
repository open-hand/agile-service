import React, { useState, useEffect, useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import { IIssueType } from '@/common/types';
import useIssueTypes from '@/hooks/data/useIssueTypes';
import { includes } from 'lodash';
import classNames from 'classnames';
import styles from './index.less';

type ChangeSelected = (code: string)=>void

interface Props {
  selectedType?: string,
  setSelectedType?: ChangeSelected,
  excludeTypes?: string[]
  brighter?: boolean
}

const IssueTypeTab: React.FC<Props> = ({
  selectedType, setSelectedType, excludeTypes = [], brighter,
}) => {
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
      const newIssueTypes = (issueTypes || []).filter((item: IIssueType) => !includes(excludeTypes, item.typeCode));
      handleSelectType((newIssueTypes && newIssueTypes[0] && newIssueTypes[0].id) as string);
    }
    if (selectedType && selected !== selectedType) {
      setSelected(selectedType);
    }
  }, [handleSelectType, selectedType, setSelectedType, issueTypes, selected, excludeTypes]);

  return (
    <div className={styles.issueTypeTab}>
      {
        (issueTypes || []).filter((item: IIssueType) => !includes(excludeTypes, item.typeCode)).map((item: IIssueType) => (
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
