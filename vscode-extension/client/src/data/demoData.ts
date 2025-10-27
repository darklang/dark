import { PackageNode, BranchNode, InstanceNode } from "../types";
import { InstanceDemoData } from "./demo";

export class DemoDataProvider {
  static getInstancesData(): InstanceNode[] {
    return InstanceDemoData.getInstancesData();
  }
}

// Re-export types for backward compatibility
export { PackageNode, BranchNode, InstanceNode };
