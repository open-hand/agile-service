import React, { useRef } from 'react';
import { observer } from 'mobx-react-lite';
import TextArea from '@/components/TextArea';
import Star from '@/components/tag/star';
import TextEditToggle from '@/components/TextEditTogglePro';
import { useLockFn } from 'ahooks';
import { useDetailContext } from '@/components/IssueDetail/context';
import { LabelLayout } from 'choerodon-ui/pro/lib/form/enum';
import styles from './index.less';

const Summary: React.FC = () => {
  const {
    store, disabledDetailEdit, outside,
  } = useDetailContext();
  const { summary, starBeacon } = store.issue;
  const handleStarClick = useLockFn(async () => {
    store.update('starBeacon', !starBeacon);
  });
  const ref = useRef<HTMLDivElement>(null);
  return (
    <div className={styles.summary} ref={ref}>
      <TextEditToggle
        className={styles.summary_edit}
        disabled={disabledDetailEdit}
        onSubmit={(value: string) => {
          store.update('summary', value);
        }}
        initValue={summary}
        alwaysRender={false}
        editor={() => (
          <TextArea
            autoSize
            maxLength={44}
            style={{
              width: ref.current ? ref.current.clientWidth - 30 : 'auto',
            }}
            required
            validationRenderer={() => '请输入概要。'}
            labelLayout={'float' as LabelLayout}
          />
        )}
      >
        <div className={styles.summary_text}>
          {summary || '无'}
        </div>
      </TextEditToggle>
      {!outside && (
        <Star
          disabled={disabledDetailEdit}
          active={starBeacon}
          onClick={handleStarClick}
          style={{ margin: '7px 5px 5px' }}
          inActiveTooltip="关注"
          activeTooltip="取消关注"
        />
      )}
    </div>
  );
};

export default observer(Summary);
