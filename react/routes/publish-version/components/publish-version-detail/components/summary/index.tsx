import React, { useRef } from 'react';
import { observer } from 'mobx-react-lite';
import TextArea from '@/components/TextArea';
import TextEditToggle from '@/components/TextEditTogglePro';
import { LabelLayout } from 'choerodon-ui/pro/lib/form/enum';
import { publishVersionApi } from '@/api';
import styles from './index.less';
import { useReleaseDetailContext } from '../../stores';

const Summary: React.FC = () => {
  const { disabled, store } = useReleaseDetailContext();
  const { name, id } = store.getCurrentData;
  const dataRef = useRef<string>(name);
  function handleCheckName(newName: string) {
    return newName === name ? new Promise((r) => r(true)) : publishVersionApi.checkAlias(newName, id).then((res: boolean) => (res ? '版本名称重复' : true));
  }
  return (
    <div className={styles.summary}>
      <TextEditToggle
        className={styles.summary_edit}
        disabled={disabled}
        onSubmit={(value: string) => {
          store.update('versionAlias', value);
        }}
        initValue={name}
        alwaysRender={false}
        editor={() => (
          <TextArea
            autoSize
            maxLength={15}
            required
            // @ts-ignore
            onInput={(e) => { dataRef.current = e.target.value; }}
            className={styles.summary_edit_textArea}
            style={{ width: '100%' }}
            validator={(value) => handleCheckName(value)}
            // validationRenderer={() => '请输入版本名称。'}
            labelLayout={'float' as LabelLayout}
          />
        )}
      >
        <div className={styles.summary_text}>
          {name || '无'}
        </div>
      </TextEditToggle>
    </div>
  );
};

export default observer(Summary);
