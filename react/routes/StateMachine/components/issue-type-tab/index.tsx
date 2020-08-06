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
  const handleSelectType = useCallback((code: string) => {
    if (setSelectedType) {
      setSelectedType(code);
    }
    setSelected(code);
  }, [setSelectedType]);
  // @ts-ignore
  const types = !isProgram ? issueTypes.filter((item:IIssueType) => item.typeCode !== 'feature') : issueTypes;

  useEffect(() => {
    if (!selectedType) {
      handleSelectType(types && types[0] && types[0].typeCode);
    }
  }, [handleSelectType, selectedType, setSelectedType, types]);

  console.log(selected, selectedType);
  return (
    <div className={styles.issueTypeTab}>
      {
        types.map((item: IIssueType) => (
          <span
            className={styles.issueTypeTabItem}
            role="none"
            onClick={() => handleSelectType(item.typeCode)}
          >
            {item.name}
          </span>
        ))
      }
    </div>
  );
};

export default observer(IssueTypeTab);
