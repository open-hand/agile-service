import React, { useEffect, useRef, useState } from 'react';
import {
  Button, Icon, Modal, SelectBox, Tooltip,
} from 'choerodon-ui/pro/lib';
import { pick } from 'lodash';
import { Tree } from 'choerodon-ui/pro';
import classnames from 'classnames';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import { randomString } from '@/utils/random';
import './DependencyTreeBase.less';

interface IDependencyTreeNodeBaseProps<T> {
  id: string
  children?: Array<T>
}
interface IDependencyTreeProps<T extends IDependencyTreeNodeBaseProps<T>> {
  data: Array<T>
  renderNode: (data: T, level: number) => React.ReactElement
}
const { TreeNode } = Tree;
/**
 * 带有连线的tree
 *
 * @returns
 */
function DependencyTree<T extends IDependencyTreeNodeBaseProps<T>>({ data: propsData, renderNode }: IDependencyTreeProps<T>) {
  const prefixCls = 'c7n-agile-dependency-tree';
  const [data, setData] = useState(propsData || []);
  useEffect(() => {
    setData(propsData);
  }, [propsData]);
  function renderTreeNodeLine(hasChildren: boolean, level: number, nodeIndex: number) {
    const lineArr = [];
    lineArr.unshift(<span
      className={`${prefixCls}-expand-line`}
      style={{
        right: hasChildren ? 17 : undefined,
        bottom: hasChildren ? 5 : undefined,
        width: hasChildren ? 16 : undefined,
        height: nodeIndex !== 0 ? 30 : undefined,
      }}
    />);
    for (let index = 2; index < level + 1; index += 1) {
      const right = 24 * index - 8 + (hasChildren ? 16 : 0);
      const width = 1;
      // if (index !== level) {
      //   width = 0;
      // }
      index === level && lineArr.push(<span
        className={`${prefixCls}-expand-line`}
        style={{
          // display: index !== level ? 'none' : undefined,
          // right: hasChildren ? 16 * level : undefined,
          // bottom: hasChildren ? 10 - 5 * level : undefined,
          right,
          height: 30,
          bottom: hasChildren ? 5 : undefined,
          width,
        }}
      />);
    }
    return lineArr;
  }
  function renderTreeNode(item: T, level: number) {
    const node = renderNode(item, level);
    const nodeStyle = pick(node.props, 'style') || {};
    return React.cloneElement(node, {
      ...node.props,
      style: {
        ...nodeStyle,
        minHeight: 30,
        height: 30,
      },
    });
  }
  function renderTree(item: T, level = 0, index = 0) {
    // let paddingLeft = level === 0 ? undefined : (28 * level - 20 * (level - 1));
    // if (!!item.children?.length && level > 1 && paddingLeft && paddingLeft > 0) {
    //   paddingLeft -= 11;
    // }
    return (
      <TreeNode
        title={renderTreeNode(item, level)}
        key={`${randomString(5)}-level-${level}-${item.id}`}
        disabled
        className={classnames({
          [`${prefixCls}-root`]: !level,
          [`${prefixCls}-leaf`]: !item.children?.length,
          [`${prefixCls}-has-leaf`]: !!item.children?.length,
        })}
        style={{
          paddingLeft: 0,
          marginLeft: 0,
        }}
        switcherIcon={(nodeProps: any) => (
          <div className={`${prefixCls}-expand`}>
            {renderTreeNodeLine(!!item.children?.length, level, index)}
            {item.children?.length ? <Icon type="navigate_next" className={`${prefixCls}-expand-icon`} /> : null}
          </div>
        )}
      >
        {item.children?.map((k, nodeIndex) => renderTree(k, level + 1, nodeIndex))}
      </TreeNode>
    );
  }

  return (
    <Tree className={`${prefixCls}`}>
      {data?.map((item, index) => renderTree(item, 0, index))}
    </Tree>
  );
}
export default DependencyTree;
