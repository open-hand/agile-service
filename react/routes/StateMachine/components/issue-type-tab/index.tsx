import React, { useState, useEffect, useCallback } from 'react';
import { useIssueTypes } from '@/hooks';
import { observer } from 'mobx-react-lite';
import { useIsProgramContext } from '@/hooks/useIsProgrom';
import { IIssueType } from '@/common/types';
import styles from './index.less';

type ChangeSelected = (code: string)=>void

interface Props {
  selectedType?: string,
  setSelectedType?: ChangeSelected,
}

const IssueTypeTab: React.FC<Props> = ({ selectedType, setSelectedType }) => {
  const [selected, setSelected] = useState(selectedType || '');
  const { isProgram } = useIsProgramContext();
  const [issueTypes] = useIssueTypes();
  const handleSelectType = useCallback((id: string) => {
    if (setSelectedType) {
      setSelectedType(id);
    }
    setSelected(id);
  }, [setSelectedType]);

  // console.log(isProgram, issueTypes);
  const types = !isProgram ? issueTypes.filter((item:IIssueType) => item.typeCode !== 'feature') : issueTypes;

  useEffect(() => {
    if (!selectedType) {
      handleSelectType(types && types[0] && types[0].id);
    }
  }, [handleSelectType, selectedType, setSelectedType, types]);

  return (
    <div className={styles.issueTypeTab}>
      {
        types.map((item: IIssueType) => (
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
