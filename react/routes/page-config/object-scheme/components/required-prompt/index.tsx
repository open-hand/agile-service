import React, { useState } from 'react';
import { IModalProps } from '@/common/types';
import { Button, CheckBox } from 'choerodon-ui/pro';

import { InjectedIntl } from 'react-intl';
import { observer } from 'mobx-react-lite';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import promptStyles from './index.less';

interface Props {
  modal?: IModalProps,
  formatMessage: InjectedIntl['formatMessage'],
  onContinue: () => void,
  promptText: string,
}
const RequiredPrompt: React.FC<Props> = ({
  modal, formatMessage, onContinue, promptText,
}) => {
  const [isPrompt, setIsPrompt] = useState<string>('true');
  const handleCancel = () => {
    modal?.close();
  };
  const handleConfirm = () => {
    localStorage.setItem('agile.page.field.setting.required.prompt', isPrompt);
    onContinue();
    modal?.close();
  };
  return (
    <>
      <div className={promptStyles.content}>
        <span className={promptStyles.text}>
          {promptText}
        </span>
        <div className={promptStyles.btn}>
          <Button onClick={handleCancel}>{formatMessage({ id: 'cancel' })}</Button>
          <Button color={'primary' as ButtonColor} onClick={handleConfirm}>{formatMessage({ id: 'confirm' })}</Button>
        </div>

      </div>
      <div className={promptStyles.footer}>
        <CheckBox value="false" onChange={setIsPrompt} defaultChecked={false}>不再提示</CheckBox>
      </div>
    </>
  );
};
export default observer(RequiredPrompt);
