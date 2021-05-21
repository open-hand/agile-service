import React, {
  Fragment, useEffect, useRef, useState,
} from 'react';
import {
  Button, Icon, Modal, SelectBox, Tooltip,
} from 'choerodon-ui/pro/lib';
import {
  useTheme,
} from '@choerodon/boot';
import { pick, set } from 'lodash';
import { Tree } from 'choerodon-ui/pro';
import classnames from 'classnames';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import { randomString } from '@/utils/random';
import './DependencyTreeBase.less';

interface IDependencyTreeNodeBaseProps<T> {
  id: string
  height?: number
  // offsetBottom?: number /** 每个连线基于原位置向下的偏移量 */
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
  const [theme] = useTheme();
  useEffect(() => {
    setData(propsData);
  }, [propsData]);
  function renderTreeNodeLine(hasChildren: boolean, nodeIndex: number, level: number, needLineLevelList: number[], height = 30, prevHeight = 30, offsetBottom = 0) {
    const lineArr = [];
    console.log('prevHeight:', prevHeight, height);
    const levelLineHeight = ((height + prevHeight) / 2);
    const levelLineBottom = offsetBottom + 5 - (height / 2);
    let selfHeight = nodeIndex === 0 ? prevHeight - 7 : levelLineHeight;
    let selfBottom = hasChildren ? 7 : levelLineBottom;
    if (theme === 'theme4') {
      selfHeight = nodeIndex === 0 ? levelLineHeight - 8 * (hasChildren ? 1 : 2) : levelLineHeight + 5;
      selfBottom = hasChildren ? 7 : levelLineBottom + 4;
      // selfHeight=hasChildren?
    }
    lineArr.push(<span
      className={`${prefixCls}-expand-line`}
      style={{
        right: hasChildren ? 17 : -5,
        bottom: selfBottom,
        width: hasChildren ? 16 : 22,
        // height: 30,
        height: selfHeight,
      }}
    />);
    needLineLevelList.forEach((item) => {
      const right = 24 * (level - item + 1) - 8 + (hasChildren ? 16 : 0);
      const width = 1;
      // if (index !== level) {
      //   width = 0;
      // }
      lineArr.push(<span
        className={`${prefixCls}-expand-line ${prefixCls}-expand-line${item} level${level}`}
        style={{
          // display: index !== level ? 'none' : undefined,
          // right: hasChildren ? 16 * level : undefined,
          // bottom: hasChildren ? 10 - 5 * level : undefined,
          right,
          height: theme === 'theme4' ? height + 5 : height,
          bottom: -height + 20,
          width,
        }}
      />);
    });

    return lineArr;
  }
  function renderTreeNode(item: T, level: number) {
    const node = renderNode(item, level);
    const nodeStyle = pick(node.props, 'style') || {};
    return React.cloneElement(node, {
      ...node.props,
      style: {
        ...nodeStyle,
        // minHeight: 30,
        // height: 30,
      },
    });
  }
  function renderTree(item: T, level = 0, index = 0, parentHasNextBrotherNode = false, needLineLevelList: number[] = [], prevHeight = 30) {
    // let paddingLeft = level === 0 ? undefined : (28 * level - 20 * (level - 1));
    // if (!!item.children?.length && level > 1 && paddingLeft && paddingLeft > 0) {
    //   paddingLeft -= 11;
    // }
    const node = renderNode(item, level);

    const { style: nodeStyle = {} } = pick(node.props, 'style') || {};
    set(item, 'height', nodeStyle.height);
    // @ts-ignore
    set(item, 'offsetBottom', node.props.offsetBottom);
    // @ts-ignore

    console.log('node', node.props.offsetBottom);
    return (
      <TreeNode
        title={node}
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
            {level !== 0 && renderTreeNodeLine(!!item.children?.length, index, level, needLineLevelList, nodeStyle.height, prevHeight, node.props.offsetBottom)}
            {item.children?.length ? <Icon type="navigate_next" className={`${prefixCls}-expand-icon`} /> : null}
          </div>
        )}
      >
        {item.children?.map((k, nodeIndex) => {
          const newNeedLineLevelList: number[] = [];
          newNeedLineLevelList.push(...needLineLevelList);
          parentHasNextBrotherNode && newNeedLineLevelList.push(level); // 父节节点有下一个兄弟节点
          return renderTree(k, level + 1, nodeIndex, nodeIndex + 1 !== item.children!.length, newNeedLineLevelList, nodeIndex === 0 ? nodeStyle.height : item.children![nodeIndex - 1].height!);
        })}
      </TreeNode>
    );
  }

  return (
    <Tree className={`${prefixCls}`}>
      {data?.map((item, index) => renderTree(item, 0, index, false, [], 30))}
    </Tree>
  );
}
interface DependencyTreeNodeProps {
  offsetBottom?: number /** 每个连线基于原位置向下的偏移量 */
  style?: React.CSSProperties

}
export const DependencyTreeNode: React.FC<DependencyTreeNodeProps> = ({
  children, offsetBottom, style, ...otherProps
}) => {
  let originNode: React.ReactElement = <span>--</span>;
  if (React.isValidElement(children)) {
    originNode = React.cloneElement(children, { ...children.props, style: { ...children.props.style, ...style } });
  }
  return React.cloneElement(<></>, { children: originNode, offsetBottom });
};
export default DependencyTree;
