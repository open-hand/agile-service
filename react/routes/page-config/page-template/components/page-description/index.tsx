import React, { useMemo, useState } from 'react';
import { observer } from 'mobx-react-lite';
import { Icon } from 'choerodon-ui/pro';
import classNames from 'classnames';
import WYSIWYGEditor from '@/components/CKEditor';
import { usePageTemplateStore } from '../../stores';
import styles from './index.less';
import useFormatMessage from '@/hooks/useFormatMessage';

function PageDescription() {
  const { pageTemplateStore, disabled } = usePageTemplateStore();
  const [hidden, setHidden] = useState(false);
  const formatMessage = useFormatMessage('agile.page');
  const handleChangeDes = (val: string) => {
    pageTemplateStore.changeTemplate(val);
  };
  const defaultValue = useMemo(() => pageTemplateStore.descriptionObj.originTemplate, [pageTemplateStore.descriptionObj.originTemplate]);
  return (
    <div className={classNames(styles.description, { [styles.hidden]: hidden })}>
      <span className={styles.title}>
        {formatMessage({ id: 'template.description.format' })}
        <Icon type={hidden ? 'expand_less' : 'expand_more'} className={classNames(styles.title_btn)} onClick={() => setHidden((oldVal) => !oldVal)} />
      </span>
      <div className={classNames(styles.edit)}>
        <WYSIWYGEditor
          style={{ height: '150px', width: '100%' }}
          disabled={disabled}
          onChange={handleChangeDes}
          value={pageTemplateStore.descriptionObj.template}
          defaultValue={defaultValue}
          placeholder="您可以在此自定义描述信息格式"
        />
      </div>
    </div>
  );
}
export default observer(PageDescription);
