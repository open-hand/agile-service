import { FieldOptionCreate } from '@/api';
import { MAX_LENGTH_FIELD_CODE, MAX_LENGTH_FIELD_NAME } from '@/constants/MAX_LENGTH';
import { usePersistFn } from 'ahooks';
import { TextField, Button } from 'choerodon-ui/pro';
import React, { memo, useState } from 'react';
import styles from './index.less';

interface AddItemProps {
  onSubmit: (data: FieldOptionCreate) => Promise<any>
  onCancel: () => void
}

const AddItem: React.FC<AddItemProps> = ({
  onSubmit, onCancel,
}) => {
  const [code, setCode] = useState('');
  const [value, setValue] = useState('');

  const handleCodeChange = usePersistFn((v) => {
    setCode(v);
  });
  const handleValueChange = usePersistFn((v) => {
    setValue(v);
  });
  const handleSave = usePersistFn(async () => {
    await onSubmit({
      code,
      value,
      sequence: 0,
      enabled: true,
    });
  });

  const renderEditor = usePersistFn(() => (
    <>
      <div className={styles.content}>
        <div className={styles.code}>
          <TextField
            value={code}
            onChange={handleCodeChange}
            maxLength={MAX_LENGTH_FIELD_CODE}
            valueChangeAction={'input' as any}
          />
        </div>
        <div className={styles.value}>
          <TextField
            value={value}
            onChange={handleValueChange}
            maxLength={MAX_LENGTH_FIELD_NAME}
            valueChangeAction={'input' as any}
          />
        </div>
      </div>
      <div className={styles.operate}>
        <Button onClick={handleSave} disabled={!code || !value}>确定</Button>
        <Button onClick={onCancel}>取消</Button>
      </div>
    </>
  ));
  return (
    <div
      className={styles.item}
    >
      {renderEditor()}
    </div>
  );
};

export default memo(AddItem);
