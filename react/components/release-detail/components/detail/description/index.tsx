import React, { ChangeEvent, useRef, useState } from 'react';
import { observer } from 'mobx-react-lite';
import { Button, Tooltip, TextArea } from 'choerodon-ui/pro/lib';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import { MAX_LENGTH_VERSION_DESCRIPTION } from '@/constants/MAX_LENGTH';
import { useReleaseDetailContext } from '../../../stores';
import styles from './index.less';
import Section from '../../section';

const Description: React.FC = () => {
  const [edit, setEdit] = useState(false);
  const editRef = useRef<string>();
  const { store, disabled } = useReleaseDetailContext();
  const { description } = store.getCurrentData;

  function handleCancel() {
    setEdit(false);
    editRef.current = '';
  }
  function handleOk() {
    store.update('description', editRef.current);
    handleCancel();
  }

  return (
    <Section
      title="描述"
      border
      buttons={
      !disabled ? (
        <Tooltip placement="topRight" autoAdjustOverflow={false} title="编辑">
          <Button
            style={{ padding: '0 6px' }}
            color={'blue' as ButtonColor}
            icon="mode_edit"
            onClick={() => {
              setEdit(true);
            }}
          />
        </Tooltip>
      ) : ''
    }
      contentClassName={styles.description}
    >
      {edit ? (
        <div className={styles.edit}>
          <TextArea
            className={styles.editor}
            autoFocus
            maxLength={MAX_LENGTH_VERSION_DESCRIPTION}
            defaultValue={description}
            onInput={(e: ChangeEvent<HTMLInputElement>) => {
              editRef.current = e.target.value;
            }}
          />
          <div className={styles.operation}>
            <Button color={'blue' as ButtonColor} onClick={handleCancel}>取消</Button>
            <Button color={'blue' as ButtonColor} onClick={handleOk}>确定</Button>
          </div>
        </div>
      ) : description}
    </Section>
  );
};
export default observer(Description);
