import React, { useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import { Loading } from '@/components';
import { omit } from 'lodash';
import { IPublishVersionTreeNode } from '@/api';
import { usePublishVersionContext } from '../../stores';
import Detail from './components/detail';
import LinkVersion, { useEditModeSectionConfig } from './components/link-version';
import styles from './index.less';
import IssueInfoTable from './components/issue-info-table';
import IssueDiffArea, { IssueDiffAreaBase } from './components/issue-diff-area';

function PublishVersionBody() {
  const context = usePublishVersionContext();
  const {
    store, menuDetail, customMenu, menuDiff, menuInfo,
  } = context;
  const menu = store.getCurrentMenu;
  const sectionProps = useEditModeSectionConfig();
  const nodeOperationProps = useMemo(() => ({ isShowEdit: (item: IPublishVersionTreeNode) => item.type === 'tag' || item.appService, isShowDel: true }), []);

  switch (menu) {
    case 'detail': {
      const customConfig = menuDetail && menuDetail([], { sectionProps, nodeOperationProps });
      const DetailContent = !customConfig ? [<Detail />, <LinkVersion sectionProps={sectionProps} nodeOperationProps={nodeOperationProps} />] : customConfig.map((item) => {
        if (item.key === 'detail') {
          return <Detail customFields={item.content as any[]} />;
        } if (item.key === 'linkVersion') {
          let { titleRight } = sectionProps;
          const { content }: { content: any } = item;
          if (content) {
            console.log('render..', content.sectionProps);
            titleRight = React.cloneElement(titleRight, { ...titleRight.props, operation: content.sectionProps.operation });
          }
          console.log('fininal', { ...sectionProps, ...content.sectionProps, titleRight });
          return <LinkVersion sectionProps={{ ...sectionProps, ...content.sectionProps, titleRight }} nodeOperationProps={content.nodeOperationProps} />;
        }
        return item.content;
      });
      return (
        <div className={styles.body}>
          {DetailContent}
        </div>
      );
    }
    case 'diff': {
      const customDiff = menuDiff && menuDiff(context);
      if (customDiff) {
        console.log('customDiff', customDiff, React.isValidElement(customDiff));
        // const menuDiffConfig = omit(customDiff, ['bottomFormContent', 'topFormContent', 'bottomFormProps']);
        return (
          <div className={styles.body_border}>
            {React.isValidElement(customDiff) ? customDiff : (
              <IssueDiffAreaBase
                bottomFormContent={customDiff.bottomFormContent}
                topFormContent={customDiff.topFormContent}
                bottomFormProps={customDiff.bottomFormProps}
                menuDiffConfig={customDiff}
              />
            )}
          </div>
        );
      }
      return (
        <div className={styles.body_border}>
          <IssueDiffArea />
        </div>
      );
    }
    case 'info': {
      const customInfo = menuInfo && menuInfo(context);

      return (
        <div className={styles.body_border}>
          <IssueInfoTable {...customInfo} />
        </div>
      );
    }
    default: {
      let defaultMenu = <div>--</div>;
      if (customMenu?.has(menu)) {
        defaultMenu = customMenu.get(menu)?.component || defaultMenu;
      }
      return defaultMenu;
    }
  }
}
export default observer(PublishVersionBody);
