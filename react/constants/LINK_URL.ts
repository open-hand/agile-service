interface IRouterList {
    knowledgeDoc: (baseId: string) => string,
}
const LINK_URL: IRouterList = {
  knowledgeDoc: (baseId: string) => `knowledge/project/doc/${baseId}`,
};
export default LINK_URL;
