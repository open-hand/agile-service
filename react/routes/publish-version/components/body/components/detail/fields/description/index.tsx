import React, { useRef } from 'react';
import { observer } from 'mobx-react-lite';
import { Tooltip } from 'choerodon-ui/pro';
import classnames from 'classnames';
import { usePublishVersionContext } from '@/routes/publish-version/stores';
import { useSize } from 'ahooks';
import Field from '../field';
import styles from './index.less';

interface Props {
  maxWidth?: number | string
}
const Description: React.FC<Props> = ({ maxWidth }) => {
  const { store, preview } = usePublishVersionContext();
  console.log('Description', maxWidth);
  const { description } = store.getCurrentData;
  return (
    <Field label="描述">
      <Tooltip title={description}>
        <span style={{ maxWidth, whiteSpace: 'nowrap' }} className={classnames(styles.description, { [styles.preview]: preview })}>{description || '无'}</span>
      </Tooltip>
    </Field>
  );
};

export default observer(Description);
