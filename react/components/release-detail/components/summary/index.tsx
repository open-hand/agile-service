import React from 'react';
import { observer } from 'mobx-react-lite';
import TextArea from '@/components/TextArea';
import TextEditToggle from '@/components/TextEditTogglePro';
import { LabelLayout } from 'choerodon-ui/pro/lib/form/enum';
import styles from './index.less';
import { useReleaseDetailContext } from '../../stores';

const Summary: React.FC = () => {
  const { disabled, store } = useReleaseDetailContext();
  const { name } = store.getCurrentData;
  return (
    <div className={styles.summary}>
      <TextEditToggle
        className={styles.summary_edit}
        disabled={disabled}
        onSubmit={(value: string) => {
          console.log('value...', value);
          // store.update('name', value);
        }}
        initValue={name}
        alwaysRender={false}
        editor={() => (
          <TextArea
            autoSize
            maxLength={15}
            required
            className={styles.summary_edit_textArea}
            style={{ width: '100%' }}
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
